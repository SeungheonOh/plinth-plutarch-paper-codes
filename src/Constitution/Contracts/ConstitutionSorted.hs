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

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

pcheck :: Term s PBool -> Term s PUnit
pcheck b = pif b (pconstant ()) perror

-- ============================================================================
-- 2. Predicate validation (integers)
-- ============================================================================

pintPredAllExpected :: Term s (PInteger :--> PInteger :--> PBuiltinList PData :--> PBool)
pintPredAllExpected = phoistAcyclic $ pfix #$ plam $ \self predKeyIdx actualInt expectedValues ->
  pelimList
    ( \expectedData expectedRest ->
        let expectedInt = pasInt # expectedData
            predResult =
              pcond
                [ (predKeyIdx #== 0, expectedInt #<= actualInt)
                , (predKeyIdx #== 1, expectedInt #>= actualInt)
                , (predKeyIdx #== 2, pnot # (expectedInt #== actualInt))
                ]
                (pconstant False)
         in predResult #&& self # predKeyIdx # actualInt # expectedRest
    )
    (pconstant True)
    expectedValues

pvalidateIntPreds :: Term s (PBuiltinList PData :--> PInteger :--> PBool)
pvalidateIntPreds = phoistAcyclic $ pfix #$ plam $ \self preds actualInt ->
  pelimList
    ( \predData predsRest ->
        let predConstrFields = psndBuiltin # (pasConstr # predData)
            predKeyIdx = pfstBuiltin # (pasConstr # (phead # predConstrFields))
            expectedValuesData = phead # (ptail # predConstrFields)
            expectedValues = pasList # expectedValuesData
         in pintPredAllExpected
              # predKeyIdx
              # actualInt
              # expectedValues
              #&& self
              # predsRest
              # actualInt
    )
    (pconstant True)
    preds

-- ============================================================================
-- 3. Predicate validation (rationals)
-- ============================================================================

pratPredAllExpected :: Term s (PInteger :--> PInteger :--> PInteger :--> PBuiltinList PData :--> PBool)
pratPredAllExpected = phoistAcyclic $ pfix #$ plam $ \self predKeyIdx actualNum actualDen expectedValues ->
  pelimList
    ( \expectedData expectedRest ->
        let expectedFields = psndBuiltin # (pasConstr # expectedData)
            expectedNum = pasInt # (phead # expectedFields)
            expectedDen = pasInt # (phead # (ptail # expectedFields))
            lhs = expectedNum * actualDen
            rhs = actualNum * expectedDen
            predResult =
              pcond
                [ (predKeyIdx #== 0, lhs #<= rhs)
                , (predKeyIdx #== 1, lhs #>= rhs)
                , (predKeyIdx #== 2, pnot # (lhs #== rhs))
                ]
                (pconstant False)
         in predResult #&& self # predKeyIdx # actualNum # actualDen # expectedRest
    )
    (pconstant True)
    expectedValues

pvalidateRatPreds :: Term s (PBuiltinList PData :--> PInteger :--> PInteger :--> PBool)
pvalidateRatPreds = phoistAcyclic $ pfix #$ plam $ \self preds actualNum actualDen ->
  pelimList
    ( \predData predsRest ->
        let predConstrFields = psndBuiltin # (pasConstr # predData)
            predKeyIdx = pfstBuiltin # (pasConstr # (phead # predConstrFields))
            expectedValuesData = phead # (ptail # predConstrFields)
            expectedValues = pasList # expectedValuesData
         in pratPredAllExpected
              # predKeyIdx
              # actualNum
              # actualDen
              # expectedValues
              #&& self
              # predsRest
              # actualNum
              # actualDen
    )
    (pconstant True)
    preds

-- ============================================================================
-- 4. ParamValue validation
-- ============================================================================

pvalidateParamValue :: Term s (PData :--> PData :--> PBool)
pvalidateParamValue =
  pfix #$ plam $ \self paramValue actualValue ->
    plet (pasConstr # paramValue) $ \constrPair ->
      let constrIdx = pfstBuiltin # constrPair
          fields = psndBuiltin # constrPair
          validateList = pfix #$ plam $ \selfList pvs actuals ->
            pelimList
              ( \pv pvRest ->
                  pelimList
                    ( \actual actualRest ->
                        self # pv # actual #&& selfList # pvRest # actualRest
                    )
                    (pconstant False)
                    actuals
              )
              (pnull # actuals)
              pvs
       in pcond
            [
              ( constrIdx #== 0
              , pvalidateIntPreds # (pasList # (phead # fields)) # (pasInt # actualValue)
              )
            ,
              ( constrIdx #== 1
              , let ratFields = psndBuiltin # (pasConstr # actualValue)
                    actualNum = pasInt # (phead # ratFields)
                    actualDen = pasInt # (phead # (ptail # ratFields))
                 in pvalidateRatPreds # (pasList # (phead # fields)) # actualNum # actualDen
              )
            ,
              ( constrIdx #== 2
              , validateList # (pasList # (phead # fields)) # (pasList # actualValue)
              )
            , (constrIdx #== 3, pconstant True)
            ]
            (pconstant False)

-- ============================================================================
-- 5. Sorted merge-join: runRules
-- ============================================================================

prunRules :: Term s (PBuiltinList PData :--> PBuiltinList (PBuiltinPair PData PData) :--> PBool)
prunRules = phoistAcyclic $ pfix #$ plam $ \self cfg cparams ->
  pelimList
    ( \cfgEntry cfgRest ->
        pelimList
          ( \cpEntry cpRest ->
              let cfgFields = psndBuiltin # (pasConstr # cfgEntry)
                  expectedPid = pasInt # (phead # cfgFields)
                  cfgParamValue = phead # (ptail # cfgFields)
                  actualPid = pasInt # (pfstBuiltin # cpEntry)
               in pif
                    (actualPid #== expectedPid)
                    ( pvalidateParamValue
                        # cfgParamValue
                        # (psndBuiltin # cpEntry)
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

pwithChangedParams :: Term s (PBuiltinList PData :--> PScriptContext :--> PBool)
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
                       in prunRules # config # (pasMap # changedParamsData)
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
  let config = pasList # configData
   in pcheck $ pwithChangedParams # config # ctx
