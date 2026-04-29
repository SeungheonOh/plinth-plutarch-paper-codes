{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Constitution.Contracts.ConstitutionSortedPlinth (
  plinthConstitutionScript,
) where

import Plutarch.Script (Script (..))
import PlutusLedgerApi.V3 (BuiltinData, ScriptContext (..), ScriptInfo (..))
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Code (getPlcNoAnn)
import PlutusTx.Prelude
import UntypedPlutusCore qualified as UPLC

import Constitution.Types.ConstitutionConfig

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

compiledCodeToScript :: PlutusTx.CompiledCode a -> Script
compiledCodeToScript code =
  let UPLC.Program ann ver body = getPlcNoAnn code
   in Script (UPLC.Program ann ver (UPLC.termMapNames UPLC.unNameDeBruijn body))

{-# INLINEABLE isNullList #-}
isNullList :: [a] -> Bool
isNullList [] = True
isNullList _ = False

{-# INLINEABLE listAll #-}
listAll :: (a -> Bool) -> [a] -> Bool
listAll _ [] = True
listAll p (x : xs) = p x && listAll p xs

-- ============================================================================
-- 2. Predicate validation (integers)
-- ============================================================================

{-# INLINEABLE intPredMeaning #-}
intPredMeaning :: PredKey -> Integer -> Integer -> Bool
intPredMeaning MinValue expected actual = expected <= actual
intPredMeaning MaxValue expected actual = expected >= actual
intPredMeaning NotEqual expected actual = expected /= actual

{-# INLINEABLE validateIntPred #-}
validateIntPred :: Integer -> (PredKey, [Integer]) -> Bool
validateIntPred actual (pk, expectedValues) =
  listAll (\expected -> intPredMeaning pk expected actual) expectedValues

{-# INLINEABLE validateIntPreds #-}
validateIntPreds :: [(PredKey, [Integer])] -> Integer -> Bool
validateIntPreds preds actual = listAll (validateIntPred actual) preds

-- ============================================================================
-- 3. Predicate validation (rationals)
-- ============================================================================

{-# INLINEABLE ratPredMeaning #-}
ratPredMeaning :: PredKey -> (Integer, Integer) -> (Integer, Integer) -> Bool
ratPredMeaning MinValue (en, ed) (an, ad) = en * ad <= an * ed
ratPredMeaning MaxValue (en, ed) (an, ad) = en * ad >= an * ed
ratPredMeaning NotEqual (en, ed) (an, ad) = en * ad /= an * ed

{-# INLINEABLE validateRatPred #-}
validateRatPred :: (Integer, Integer) -> (PredKey, [(Integer, Integer)]) -> Bool
validateRatPred actual (pk, expectedValues) =
  listAll (\expected -> ratPredMeaning pk expected actual) expectedValues

{-# INLINEABLE validateRatPreds #-}
validateRatPreds :: [(PredKey, [(Integer, Integer)])] -> (Integer, Integer) -> Bool
validateRatPreds preds actual = listAll (validateRatPred actual) preds

-- ============================================================================
-- 4. ParamValue validation
-- ============================================================================

{-# INLINEABLE validateParamValue #-}
validateParamValue :: ParamValue -> BuiltinData -> Bool
validateParamValue (ParamInteger preds) d =
  validateIntPreds preds (unsafeFromBuiltinData d)
validateParamValue (ParamRational preds) d =
  validateRatPreds preds (unsafeFromBuiltinData d)
validateParamValue (ParamList paramValues) d =
  validateParamValues paramValues (unsafeFromBuiltinData d)
 where
  validateParamValues :: [ParamValue] -> [BuiltinData] -> Bool
  validateParamValues (pv : pvs) (dd : ds) =
    validateParamValue pv dd && validateParamValues pvs ds
  validateParamValues [] ds = isNullList ds
  validateParamValues _ [] = False
validateParamValue ParamAny _ = True

-- ============================================================================
-- 5. Sorted merge-join: runRules
-- ============================================================================

{-# INLINEABLE runRules #-}
runRules
  :: [(Integer, ParamValue)]
  -> [(BuiltinData, BuiltinData)]
  -> Bool
runRules ((expectedPid, paramValue) : cfgRest) cparams@((actualPidData, actualValueData) : cparamsRest) =
  let actualPid = Builtins.unsafeDataAsI actualPidData
   in case compare actualPid expectedPid of
        EQ ->
          validateParamValue paramValue actualValueData
            && runRules cfgRest cparamsRest
        GT ->
          runRules cfgRest cparams
        LT ->
          False
runRules _ cparams = isNullList cparams

-- ============================================================================
-- 6. Governance action extraction (BuiltinData-level, follows original)
-- ============================================================================

{-# INLINEABLE scriptContextToScriptInfo #-}
scriptContextToScriptInfo :: BuiltinData -> BuiltinData
scriptContextToScriptInfo ctxData =
  BI.head (BI.tail (BI.tail (BI.snd (BI.unsafeDataAsConstr ctxData))))

{-# INLINEABLE scriptInfoToProposalProcedure #-}
scriptInfoToProposalProcedure :: BuiltinData -> BuiltinData
scriptInfoToProposalProcedure siData =
  let si = BI.unsafeDataAsConstr siData
   in if BI.fst si `Builtins.equalsInteger` 5
        then BI.head (BI.tail (BI.snd si))
        else traceError "C2"

{-# INLINEABLE proposalProcedureToGovernanceAction #-}
proposalProcedureToGovernanceAction :: BuiltinData -> BuiltinData
proposalProcedureToGovernanceAction ppData =
  BI.head (BI.tail (BI.tail (BI.snd (BI.unsafeDataAsConstr ppData))))

{-# INLINEABLE governanceActionToChangedParams #-}
governanceActionToChangedParams :: BuiltinData -> Maybe [(BuiltinData, BuiltinData)]
governanceActionToChangedParams gaData =
  let ga = BI.unsafeDataAsConstr gaData
      gaConstr = BI.fst ga
   in if gaConstr `Builtins.equalsInteger` 0
        then Just (Builtins.unsafeDataAsMap (BI.head (BI.tail (BI.snd ga))))
        else
          if gaConstr `Builtins.equalsInteger` 2
            then Nothing
            else traceError "C1"

{-# INLINEABLE withChangedParams #-}
withChangedParams :: [(Integer, ParamValue)] -> BuiltinData -> Bool
withChangedParams config ctxData =
  let siData = scriptContextToScriptInfo ctxData
      ppData = scriptInfoToProposalProcedure siData
      gaData = proposalProcedureToGovernanceAction ppData
   in case governanceActionToChangedParams gaData of
        Just cparams -> runRules config cparams
        Nothing -> True

-- ============================================================================
-- 7. Entry point and compiled script
-- ============================================================================

{-# INLINEABLE mkConstitutionValidator #-}
mkConstitutionValidator :: BuiltinData -> BuiltinData -> ()
mkConstitutionValidator configData ctxData =
  let config :: [(Integer, ParamValue)]
      config = unsafeFromBuiltinData configData
   in if withChangedParams config ctxData then () else error ()

plinthConstitutionScript :: Script
plinthConstitutionScript =
  compiledCodeToScript $$(PlutusTx.compile [||mkConstitutionValidator||])
