{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Constitution.Contracts.ConstitutionSorted (
  mkConstitutionValidator,
) where

import Plutarch.LedgerApi.V3
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Constitution.Types.ConstitutionConfig (
  PConfigEntry (..),
  PIntPredEntry (..),
  PParamValue (..),
  PPredKey (..),
  PRatPair (..),
  PRatPredEntry (..),
 )

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

pcheck :: Term s PBool -> Term s PUnit
pcheck b = pif b (pconstant ()) perror

-- ============================================================================
-- 2. Predicate validation (integers)
--
-- `papplyIntPred` operates on a typed `PPredKey`; nothing is decoded from raw
-- `PData` by hand.
-- ============================================================================

pintPredAllExpected
  :: Term
       s
       ( PPredKey
           :--> PInteger
           :--> PBuiltinList (PAsData PInteger)
           :--> PBool
       )
pintPredAllExpected = phoistAcyclic $ pfix #$ plam $ \self pk actual expecteds ->
  pelimList
    ( \expected rest ->
        pmatch
          pk
          ( \case
              PMinValue -> pfromData expected #<= actual
              PMaxValue -> pfromData expected #>= actual
              PNotEqual -> pnot # (pfromData expected #== actual)
          )
          #&& self
          # pk
          # actual
          # rest
    )
    (pconstant True)
    expecteds

pvalidateIntPreds
  :: Term
       s
       ( PBuiltinList (PAsData PIntPredEntry)
           :--> PInteger
           :--> PBool
       )
pvalidateIntPreds = phoistAcyclic $ pfix #$ plam $ \self preds actual ->
  pelimList
    ( \entry rest ->
        pmatch (pfromData entry) $ \(PIntPredEntry pk exps) ->
          pintPredAllExpected
            # pfromData pk
            # actual
            # pfromData exps
            #&& self
            # rest
            # actual
    )
    (pconstant True)
    preds

-- ============================================================================
-- 3. Predicate validation (rationals)
-- ============================================================================

pratPredAllExpected
  :: Term
       s
       ( PPredKey
           :--> PInteger
           :--> PInteger
           :--> PBuiltinList (PAsData PRatPair)
           :--> PBool
       )
pratPredAllExpected = phoistAcyclic $ pfix #$ plam $ \self pk actualNum actualDen expecteds ->
  pelimList
    ( \pairD rest ->
        pmatch (pfromData pairD) $ \(PRatPair{prpNum, prpDen}) ->
          pmatch
            pk
            ( \case
                PMinValue -> (pfromData prpNum * actualDen) #<= (actualNum * pfromData prpDen)
                PMaxValue -> (pfromData prpNum * actualDen) #>= (actualNum * pfromData prpDen)
                PNotEqual -> pnot # ((pfromData prpNum * actualDen) #== (actualNum * pfromData prpDen))
            )
            #&& self
            # pk
            # actualNum
            # actualDen
            # rest
    )
    (pconstant True)
    expecteds

pvalidateRatPreds
  :: Term
       s
       ( PBuiltinList (PAsData PRatPredEntry)
           :--> PInteger
           :--> PInteger
           :--> PBool
       )
pvalidateRatPreds = phoistAcyclic $ pfix #$ plam $ \self preds actualNum actualDen ->
  pelimList
    ( \entry rest ->
        pmatch (pfromData entry) $ \(PRatPredEntry pk exps) ->
          pratPredAllExpected
            # pfromData pk
            # actualNum
            # actualDen
            # pfromData exps
            #&& self
            # rest
            # actualNum
            # actualDen
    )
    (pconstant True)
    preds

-- ============================================================================
-- 4. ParamValue validation
--
-- `pvalidateParamValue` takes a typed `PParamValue`. The actual changed-value
-- is still passed as `PData` because its shape depends on which constructor
-- of `PParamValue` we are looking at.
-- ============================================================================

pvalidateParamValue :: Term s (PParamValue :--> PData :--> PBool)
pvalidateParamValue =
  pfix #$ plam $ \self paramValue actualValue ->
    pmatch paramValue $ \case
      PParamInteger preds ->
        pvalidateIntPreds # pfromData preds # (pasInt # actualValue)
      PParamRational preds ->
        let ratList = pasList # actualValue
            actualNum = pasInt # (phead # ratList)
            actualDen = pasInt # (phead # (ptail # ratList))
         in pvalidateRatPreds # pfromData preds # actualNum # actualDen
      PParamList pvList ->
        let actualList = pasList # actualValue
            validateList = pfix #$ plam $ \selfList pvs actuals ->
              pelimList
                ( \pvD pvRest ->
                    pelimList
                      ( \actual actualRest ->
                          self
                            # pfromData pvD
                            # actual
                            #&& selfList
                            # pvRest
                            # actualRest
                      )
                      (pconstant False)
                      actuals
                )
                (pnull # actuals)
                pvs
         in validateList # pfromData pvList # actualList
      PParamAny -> pconstant True

-- ============================================================================
-- 5. Sorted merge-join: runRules
--
-- The configuration is a typed list of `PConfigEntry` (the wire-level pair
-- `(Integer, ParamValue)`), and the changed-params side is a list of typed
-- builtin pairs.
-- ============================================================================

prunRules
  :: Term
       s
       ( PBuiltinList (PAsData PConfigEntry)
           :--> PBuiltinList (PBuiltinPair (PAsData PInteger) PData)
           :--> PBool
       )
prunRules = phoistAcyclic $ pfix #$ plam $ \self cfg cparams ->
  pelimList
    ( \cfgEntry cfgRest ->
        pelimList
          ( \cpEntry cpRest ->
              pmatch (pfromData cfgEntry) $ \(PConfigEntry{pceParamId, pceParamValue}) ->
                let expectedPid = pfromData pceParamId
                    actualPid = pfromData (pfstBuiltin # cpEntry)
                    actualValue = psndBuiltin # cpEntry
                 in pif
                      (actualPid #== expectedPid)
                      ( pvalidateParamValue
                          # pfromData pceParamValue
                          # actualValue
                          #&& self
                          # cfgRest
                          # cpRest
                      )
                      ( pif
                          (actualPid #> expectedPid)
                          (self # cfgRest # cparams)
                          (pconstant False)
                      )
          )
          (pconstant True)
          cparams
    )
    (pnull # cparams)
    cfg

-- ============================================================================
-- 6. Governance action extraction
-- ============================================================================

pwithChangedParams
  :: Term
       s
       ( PBuiltinList (PAsData PConfigEntry)
           :--> PScriptContext
           :--> PBool
       )
pwithChangedParams = phoistAcyclic $ plam $ \config ctx ->
  pmatch ctx $ \(PScriptContext{pscriptContext'scriptInfo}) ->
    pmatch pscriptContext'scriptInfo $ \case
      PProposingScript _ pp ->
        pmatch pp $ \(PProposalProcedure{pproposalProcedure'governanceAction}) ->
          plet (pasConstr # pforgetData (pdata pproposalProcedure'governanceAction)) $ \govConstr ->
            let govIdx = pfstBuiltin # govConstr
                govFields = psndBuiltin # govConstr
             in pcond
                  [
                    ( govIdx #== 0
                    , let changedParamsData = phead # (ptail # govFields)
                          changedParams =
                            punsafeCoerce
                              @(PBuiltinList (PBuiltinPair (PAsData PInteger) PData))
                              (pasMap # changedParamsData)
                       in prunRules # config # changedParams
                    )
                  , (govIdx #== 2, pconstant True)
                  ]
                  (ptraceInfoError "C1")
      _ -> ptraceInfoError "C2"

-- ============================================================================
-- 7. Main validator
-- ============================================================================

mkConstitutionValidator :: Term s (PData :--> PScriptContext :--> PUnit)
mkConstitutionValidator = plam $ \configData ctx ->
  let config =
        punsafeCoerce @(PBuiltinList (PAsData PConfigEntry)) (pasList # configData)
   in pcheck $ pwithChangedParams # config # ctx
