{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Voting.Contracts.Voting (
  mkVotingValidator,
) where

import Plutarch.LedgerApi.V3
import Plutarch.Prelude
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

pcheck :: Term s PBool -> Term s PUnit
pcheck b = pif b (pconstant ()) perror

-- ============================================================================
-- 2. Utility functions
-- ============================================================================

pvalueHasNFT :: Term s (PData :--> PData :--> PData :--> PBool)
pvalueHasNFT = phoistAcyclic $ plam $ \csData tnData valueData ->
  let outerMap = pasMap # valueData
   in pfindCSEntry # csData # tnData # outerMap

pfindCSEntry :: Term s (PData :--> PData :--> PBuiltinList (PBuiltinPair PData PData) :--> PBool)
pfindCSEntry = phoistAcyclic $ pfix #$ plam $ \self csData tnData entries ->
  pelimList
    ( \pair rest ->
        pif
          (pfstBuiltin # pair #== csData)
          (pfindTNEntry # tnData # (pasMap # (psndBuiltin # pair)))
          (self # csData # tnData # rest)
    )
    (pconstant False)
    entries

pfindTNEntry :: Term s (PData :--> PBuiltinList (PBuiltinPair PData PData) :--> PBool)
pfindTNEntry = phoistAcyclic $ pfix #$ plam $ \self tnData tokens ->
  pelimList
    ( \tk rest ->
        pif
          (pfstBuiltin # tk #== tnData)
          (pasInt # (psndBuiltin # tk) #== 1)
          (self # tnData # rest)
    )
    (pconstant False)
    tokens

pinputHasNFT :: Term s (PData :--> PData :--> PAsData PTxInInfo :--> PBool)
pinputHasNFT = phoistAcyclic $ plam $ \csData tnData inpData ->
  pmatch (pfromData inpData) $ \(PTxInInfo{ptxInInfo'resolved}) ->
    pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'value}) ->
      pvalueHasNFT # csData # tnData # pforgetData (pdata (pfromData ptxOut'value))

panyInputHasNFT :: Term s (PData :--> PData :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
panyInputHasNFT = phoistAcyclic $ pfix #$ plam $ \self csData tnData inputs ->
  pelimList
    ( \inp rest ->
        pif
          (pinputHasNFT # csData # tnData # inp)
          (pconstant True)
          (self # csData # tnData # rest)
    )
    (pconstant False)
    inputs

-- ============================================================================
-- 3. Main validator logic
-- ============================================================================

pvotingValidator :: Term s (PData :--> PData :--> PTxInfo :--> PUnit)
pvotingValidator = phoistAcyclic $ plam $ \csData tnData txInfo ->
  let inputs = pfromData $ pmatch txInfo $ \txI -> ptxInfo'inputs txI
   in pcheck $ panyInputHasNFT # csData # tnData # inputs

-- ============================================================================
-- 4. Entry point
-- ============================================================================

mkVotingValidator :: Term s (PData :--> PData :--> PScriptContext :--> PUnit)
mkVotingValidator = plam $ \csData tnData ctx ->
  pmatch ctx $ \(PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo}) ->
    pmatch pscriptContext'scriptInfo $ \case
      PVotingScript _ ->
        pvotingValidator # csData # tnData # pscriptContext'txInfo
      _ -> perror

-- BUG: a local PUserList type with no `PListLike` instance, plus a
-- 'plength # xs' call. The unsatisfiable constraint fires the E21
-- Plutarch diagnostic.
data PUserList (s :: S)
  = PUNil
  | PUCons (Term s (PAsData PInteger)) (Term s (PAsData PUserList))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via DeriveAsDataStruct PUserList

puserListLength :: Term s (PUserList :--> PInteger)
puserListLength = plam $ \xs -> plength # xs
