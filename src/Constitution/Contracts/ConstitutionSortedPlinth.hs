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
import PlutusLedgerApi.Data.V3 (BuiltinData)
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

{-# INLINEABLE listHead #-}
listHead :: [a] -> a
listHead (x : _) = x
listHead [] = traceError "empty list"

{-# INLINEABLE listTail #-}
listTail :: [a] -> [a]
listTail (_ : xs) = xs
listTail [] = traceError "empty list"

-- ============================================================================
-- 2. Predicate validation (integers)
-- ============================================================================

{-# INLINEABLE intPredAllExpected #-}
intPredAllExpected :: PredKey -> Integer -> [Integer] -> Bool
intPredAllExpected pk actual expectedValues =
  listAll (\expected -> applyPred pk expected actual) expectedValues
 where
  applyPred MinValue expected act = expected <= act
  applyPred MaxValue expected act = expected >= act
  applyPred NotEqual expected act = expected /= act

{-# INLINEABLE validateIntPreds #-}
validateIntPreds :: [(PredKey, [Integer])] -> Integer -> Bool
validateIntPreds preds actual =
  listAll (\(pk, exps) -> intPredAllExpected pk actual exps) preds

-- ============================================================================
-- 3. Predicate validation (rationals)
-- ============================================================================

{-# INLINEABLE ratPredAllExpected #-}
ratPredAllExpected :: PredKey -> Integer -> Integer -> [(Integer, Integer)] -> Bool
ratPredAllExpected pk actualNum actualDen expectedValues =
  listAll (\(en, ed) -> applyPred pk (en * actualDen) (actualNum * ed)) expectedValues
 where
  applyPred MinValue lhs rhs = lhs <= rhs
  applyPred MaxValue lhs rhs = lhs >= rhs
  applyPred NotEqual lhs rhs = lhs /= rhs

{-# INLINEABLE validateRatPreds #-}
validateRatPreds :: [(PredKey, [(Integer, Integer)])] -> Integer -> Integer -> Bool
validateRatPreds preds actualNum actualDen =
  listAll (\(pk, exps) -> ratPredAllExpected pk actualNum actualDen exps) preds

-- ============================================================================
-- 4. ParamValue validation
-- ============================================================================

{-# INLINEABLE validateParamValue #-}
validateParamValue :: BuiltinData -> BuiltinData -> Bool
validateParamValue pvData d =
  let pv :: ParamValue
      pv = unsafeFromBuiltinData pvData
   in case pv of
        ParamInteger preds ->
          validateIntPreds preds (unsafeFromBuiltinData d)
        ParamRational preds ->
          let actualList = unsafeFromBuiltinData @[Integer] d
           in validateRatPreds preds (listHead actualList) (listHead (listTail actualList))
        ParamList paramValues ->
          validateParamValues paramValues (unsafeFromBuiltinData d)
         where
          validateParamValues :: [ParamValue] -> [BuiltinData] -> Bool
          validateParamValues (pv' : pvs) (dd : ds) =
            validateParamValue (toBuiltinData pv') dd && validateParamValues pvs ds
          validateParamValues [] ds = isNullList ds
          validateParamValues _ [] = False
        ParamAny -> True

-- ============================================================================
-- 5. Sorted merge-join: runRules
-- ============================================================================

{-# INLINEABLE runRules #-}
runRules
  :: [(Integer, BuiltinData)]
  -> [(BuiltinData, BuiltinData)]
  -> Bool
runRules ((expectedPid, paramValueData) : cfgRest) cparams@((actualPidData, actualValueData) : cparamsRest) =
  let actualPid = Builtins.unsafeDataAsI actualPidData
   in case compare actualPid expectedPid of
        EQ ->
          validateParamValue paramValueData actualValueData
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
withChangedParams :: [(Integer, BuiltinData)] -> BuiltinData -> Bool
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
  let config :: [(Integer, BuiltinData)]
      config = unsafeFromBuiltinData configData
   in if withChangedParams config ctxData then () else error ()

plinthConstitutionScript :: Script
plinthConstitutionScript =
  compiledCodeToScript $$(PlutusTx.compile [||mkConstitutionValidator||])
