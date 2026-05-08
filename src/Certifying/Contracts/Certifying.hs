{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Certifying.Contracts.Certifying (
  mkCertifyingValidator,
) where

import Plutarch.LedgerApi.V3
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

pcheck :: Term s PBool -> Term s PUnit
pcheck b = pif b (pconstant ()) perror

-- ============================================================================
-- 2. Utility functions
-- ============================================================================

pisEntirelyAfter :: Term s (PTxInfo :--> PInteger :--> PBool)
pisEntirelyAfter = phoistAcyclic $ plam $ \txInfo threshold ->
  pmatch txInfo $ \txI ->
    pmatch (ptxInfo'validRange txI) $ \(PInterval lb _) ->
      pmatch lb $ \(PLowerBound ext closure) ->
        pmatch ext $ \case
          PFinite t ->
            let time = pfromData (punsafeCoerce @(PAsData PInteger) t)
             in pif (pfromData closure) (time #> threshold) (time #>= threshold)
          PNegInf -> pconstant False
          PPosInf -> pconstant True

pisDelegateToAbstain :: Term s (PData :--> PBool)
pisDelegateToAbstain = phoistAcyclic $ plam $ \delegData ->
  plet (pasConstr # delegData) $ \delegPair ->
    let delegIdx = pfstBuiltin # delegPair
        delegFields = psndBuiltin # delegPair
     in pif
          (delegIdx #== 1)
          ( let drepData = phead # delegFields
                drepIdx = pfstBuiltin # (pasConstr # drepData)
             in drepIdx #== 1
          )
          (pconstant False)

-- ============================================================================
-- 3. Main validator logic
-- ============================================================================

pcertifyingValidator :: Term s (PInteger :--> PData :--> PTxInfo :--> PUnit)
pcertifyingValidator = phoistAcyclic $ plam $ \expiration certData txInfo ->
  plet (pasConstr # certData) $ \certPair ->
    let certIdx = pfstBuiltin # certPair
        certFields = psndBuiltin # certPair
     in pcheck $
          pcond
            [ (certIdx #== 0, pconstant True)
            , (certIdx #== 1, pisEntirelyAfter # txInfo # expiration)
            ,
              ( certIdx #== 2
              , pisDelegateToAbstain # (phead # (ptail # certFields))
              )
            ,
              ( certIdx #== 3
              , pisDelegateToAbstain # (phead # (ptail # certFields))
              )
            ]
            (pconstant False)

-- ============================================================================
-- 4. Entry point
-- ============================================================================

mkCertifyingValidator :: Term s (PData :--> PScriptContext :--> PUnit)
mkCertifyingValidator = plam $ \expirationData ctx ->
  let expiration = pasInt # expirationData
   in pmatch ctx $ \(PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo}) ->
        pmatch pscriptContext'scriptInfo $ \case
          PCertifyingScript _idx certData ->
            pcertifyingValidator # expiration # pforgetData (pdata certData) # pscriptContext'txInfo
          _ -> perror
