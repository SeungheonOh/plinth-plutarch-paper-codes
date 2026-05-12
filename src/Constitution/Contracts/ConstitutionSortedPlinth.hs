{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:no-preserve-logging #-}

module Constitution.Contracts.ConstitutionSortedPlinth (
  plinthConstitutionScript,
) where

import Plinth.Plugin
import Plutarch.Script (Script (..))
import PlutusLedgerApi.Data.V3 (
  ScriptContext,
  getChangedParameters,
  ppGovernanceAction,
  scriptContextScriptInfo,
  pattern ParameterChange,
  pattern ProposingScript,
  pattern TreasuryWithdrawals,
 )
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Code (getPlcNoAnn)
import PlutusTx.Data.AssocMap (Map)
import PlutusTx.Data.AssocMap qualified as DMap
import PlutusTx.Data.List (List)
import PlutusTx.Data.List qualified as DList
import PlutusTx.Prelude
import UntypedPlutusCore qualified as UPLC

import Constitution.Types.ConstitutionConfig (
  ParamValueD,
  PredKeyD,
  pattern MaxValueD,
  pattern MinValueD,
  pattern NotEqualD,
  pattern ParamAnyD,
  pattern ParamIntegerD,
  pattern ParamListD,
  pattern ParamRationalD,
 )

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

compiledCodeToScript :: PlutusTx.CompiledCode a -> Script
compiledCodeToScript code =
  let UPLC.Program ann ver body = getPlcNoAnn code
   in Script (UPLC.Program ann ver (UPLC.termMapNames UPLC.unNameDeBruijn body))

-- ============================================================================
-- 2. Predicate validation (integers)
-- ============================================================================

{-# INLINEABLE intPredAllExpected #-}
intPredAllExpected :: PredKeyD -> Integer -> List Integer -> Bool
intPredAllExpected pk actual = case pk of
  MinValueD -> DList.all (<= actual)
  MaxValueD -> DList.all (>= actual)
  NotEqualD -> DList.all (/= actual)

{-# INLINEABLE validateIntPreds #-}
validateIntPreds :: List (PredKeyD, List Integer) -> Integer -> Bool
validateIntPreds preds actual =
  DList.all (\(pk, exps) -> intPredAllExpected pk actual exps) preds

-- ============================================================================
-- 3. Predicate validation (rationals)
-- ============================================================================

{-# INLINEABLE ratPredAllExpected #-}
ratPredAllExpected :: PredKeyD -> Integer -> Integer -> List (Integer, Integer) -> Bool
ratPredAllExpected pk actualNum actualDen = case pk of
  MinValueD -> DList.all (\(en, ed) -> en * actualDen <= actualNum * ed)
  MaxValueD -> DList.all (\(en, ed) -> en * actualDen >= actualNum * ed)
  NotEqualD -> DList.all (\(en, ed) -> en * actualDen /= actualNum * ed)

{-# INLINEABLE validateRatPreds #-}
validateRatPreds :: List (PredKeyD, List (Integer, Integer)) -> Integer -> Integer -> Bool
validateRatPreds preds actualNum actualDen =
  DList.all (\(pk, exps) -> ratPredAllExpected pk actualNum actualDen exps) preds

-- ============================================================================
-- 4. ParamValue validation
-- ============================================================================

{-# INLINEABLE validateParamValue #-}
validateParamValue :: BuiltinData -> BuiltinData -> Bool
validateParamValue pvData actualData =
  case unsafeFromBuiltinData @ParamValueD pvData of
    ParamIntegerD preds ->
      validateIntPreds preds (unsafeFromBuiltinData actualData)
    ParamRationalD preds ->
      let actualList = unsafeFromBuiltinData @(List Integer) actualData
          actualNum = DList.head actualList
          actualDen = DList.head (DList.tail actualList)
       in validateRatPreds preds actualNum actualDen
    ParamListD pvList ->
      validateParamValues pvList (unsafeFromBuiltinData @(List BuiltinData) actualData)
     where
      validateParamValues :: List BuiltinData -> List BuiltinData -> Bool
      validateParamValues pvs ds =
        DList.caseList'
          (DList.null ds)
          ( \pv pvRest ->
              DList.caseList'
                False
                (\d dRest -> validateParamValue pv d && validateParamValues pvRest dRest)
                ds
          )
          pvs
    ParamAnyD -> True

-- ============================================================================
-- 5. Sorted merge-join: runRules
--
-- Config side is a data-encoded `List (Integer, BuiltinData)`; changed-params
-- side is the underlying `BuiltinList (BuiltinPair BuiltinData BuiltinData)`
-- of a `DMap.Map`. Nothing is decoded into a regular Haskell list.
-- ============================================================================

{-# INLINEABLE runRules #-}
runRules :: List (Integer, BuiltinData) -> Map Integer BuiltinData -> Bool
runRules config changedParams = go config (DMap.toBuiltinList changedParams)
 where
  go cfg cparams =
    Builtins.matchList'
      cparams
      True
      ( \firstPair restPairs ->
          let actualPid = Builtins.unsafeDataAsI (BI.fst firstPair)
              actualValueData = BI.snd firstPair
              advance cfg' =
                DList.caseList'
                  False
                  ( \(expectedPid, paramValueData) cfgRest ->
                      if actualPid == expectedPid
                        then
                          validateParamValue paramValueData actualValueData
                            && go cfgRest restPairs
                        else actualPid > expectedPid && advance cfgRest
                  )
                  cfg'
           in advance cfg
      )

-- ============================================================================
-- 6. Governance action dispatch
-- ============================================================================

{-# INLINEABLE withChangedParams #-}
withChangedParams :: List (Integer, BuiltinData) -> ScriptContext -> Bool
withChangedParams config ctx =
  case scriptContextScriptInfo ctx of
    ProposingScript _ proposal ->
      case ppGovernanceAction proposal of
        ParameterChange _ cp _ ->
          let changedParams =
                unsafeFromBuiltinData @(Map Integer BuiltinData)
                  (getChangedParameters cp)
           in runRules config changedParams
        TreasuryWithdrawals _ _ -> True
        _ -> error ()
    _ -> error ()

-- ============================================================================
-- 7. Entry point and compiled script
-- ============================================================================

{-# INLINEABLE mkConstitutionValidator #-}
mkConstitutionValidator :: BuiltinData -> BuiltinData -> ()
mkConstitutionValidator configData ctxData =
  let config = unsafeFromBuiltinData @(List (Integer, BuiltinData)) configData
      ctx = unsafeFromBuiltinData @ScriptContext ctxData
   in if withChangedParams config ctx then () else error ()

plinthConstitutionScript :: Script
plinthConstitutionScript =
  compiledCodeToScript $ plinthc mkConstitutionValidator
