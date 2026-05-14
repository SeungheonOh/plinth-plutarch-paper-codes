{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Constitution.Test.ConstitutionSorted (
  mkConstitutionTests,
  mkConstitutionConformanceTests,
  constitutionBenchScenarios,
) where

import Data.Either (isRight)
import Data.List (nub, sort)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Script (Script)
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import ProgrammableTokens.Test.ScriptContext.Builder (
  buildScriptContext,
  withProposingScript,
 )
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Constitution.Types.ConstitutionConfig

-- ============================================================================
-- Context construction (mirrors PlutusLedgerApi.V3.ArbitraryContexts)
-- ============================================================================

mkFakeParameterChangeContext :: (PlutusTx.ToData b) => [(Integer, b)] -> ScriptContext
mkFakeParameterChangeContext params =
  let changedParams = ChangedParameters (PlutusTx.toBuiltinData (Map.unsafeFromList params))
      govAction = ParameterChange Nothing changedParams Nothing
      proposal = ProposalProcedure 0 (PubKeyCredential "") govAction
   in buildScriptContext (withProposingScript (PlutusTx.toBuiltinData ()) proposal)

mkFakeContextFromGovAction :: GovernanceAction -> ScriptContext
mkFakeContextFromGovAction govAction =
  let proposal = ProposalProcedure 0 (PubKeyCredential "") govAction
   in buildScriptContext (withProposingScript (PlutusTx.toBuiltinData ()) proposal)

-- ============================================================================
-- Rational encoding (matches NonCanonicalRational: encoded as [n, d])
-- ============================================================================

rat :: Integer -> Integer -> [Integer]
rat n d = [n, d]

-- ============================================================================
-- Constitution config (real Cardano guardrail params)
-- ============================================================================

constitutionConfig :: [(Integer, ParamValue)]
constitutionConfig =
  [ (0, ParamInteger [(MinValue, [30]), (MaxValue, [1_000])])
  , (1, ParamInteger [(MinValue, [100_000]), (MaxValue, [10_000_000])])
  , (5, ParamInteger [(MinValue, [1_000_000]), (MaxValue, [5_000_000])])
  , (10, ParamRational [(MinValue, [(1, 1000)]), (MaxValue, [(5, 1000)])])
  , (17, ParamInteger [(MinValue, [3_000]), (MaxValue, [6_500]), (NotEqual, [0])])
  ,
    ( 19
    , ParamList
        [ ParamRational [(MinValue, [(400, 10_000)]), (MaxValue, [(2_000, 10_000)])]
        , ParamRational [(MinValue, [(500, 10_000_000)]), (MaxValue, [(2_000, 10_000_000)])]
        ]
    )
  ]

-- ============================================================================
-- Ported test contexts from UnitTests.hs
-- ============================================================================

-- test_pos: pos1 - multiple integer params in valid range
pos1 :: ScriptContext
pos1 =
  mkFakeParameterChangeContext @Integer
    [ (0, 30)
    , (1, 10_000_000)
    , (17, 6_250)
    ]

-- test_pos: pos2 - single rational param in range
pos2 :: ScriptContext
pos2 =
  mkFakeParameterChangeContext
    [(10, rat 1 1000)]

-- test_pos: pos3 - list param (execution unit prices) both sub-params in range
pos3 :: ScriptContext
pos3 =
  mkFakeParameterChangeContext
    [
      ( 19
      ,
        [ rat 2_000 10_000
        , rat 500 10_000_000
        ]
      )
    ]

-- test_pos: pos4 - empty params
pos4 :: ScriptContext
pos4 = mkFakeParameterChangeContext @Integer []

-- test_pos: pos5 - treasury withdrawal (always accepted)
pos5 :: ScriptContext
pos5 = mkFakeContextFromGovAction (TreasuryWithdrawals Map.empty Nothing)

-- test_neg: neg1 - param 0 below min + unknown param 2
neg1 :: ScriptContext
neg1 =
  mkFakeParameterChangeContext @Integer
    [ (0, 29)
    , (1, 10_000_000)
    , (2, -10_000_000_000)
    , (17, 6_250)
    ]

-- test_neg: neg2 - param 0 below min
neg2 :: ScriptContext
neg2 =
  mkFakeParameterChangeContext @Integer
    [ (0, 29)
    , (1, 10_000_000)
    , (17, 6_251)
    ]

-- test_neg: neg3 - unknown negative param
neg3 :: ScriptContext
neg3 =
  mkFakeParameterChangeContext @Integer
    [(-1, -1_000)]

-- test_neg: neg4 - type mismatch (integer instead of rational for param 10)
neg4 :: ScriptContext
neg4 =
  mkFakeParameterChangeContext @Integer
    [(10, 1)]

-- test_neg: neg5 - type mismatch (rational instead of integer for param 0)
neg5 :: ScriptContext
neg5 =
  mkFakeParameterChangeContext
    [(0, rat 1 1)]

-- test_neg: neg6 - rational out of limits
neg6 :: ScriptContext
neg6 =
  mkFakeParameterChangeContext
    [(10, rat 0 1000)]

-- test_neg: neg7 - list param with one sub-param out of range
neg7 :: ScriptContext
neg7 =
  mkFakeParameterChangeContext
    [
      ( 19
      ,
        [ rat 2_000 10_000
        , rat 0 10_000_000
        ]
      )
    ]

-- test_neg: neg8 - list param with too many sub-params
neg8 :: ScriptContext
neg8 =
  mkFakeParameterChangeContext
    [
      ( 19
      ,
        [ rat 2_000 10_000
        , rat 500 10_000_000
        , rat 500 10_000_000
        ]
      )
    ]

-- test_neg: neg9 - list param with too few sub-params
neg9 :: ScriptContext
neg9 =
  mkFakeParameterChangeContext
    [
      ( 19
      , [rat 2_000 10_000]
      )
    ]

-- test_neg: neg10 - list param with empty list
neg10 :: ScriptContext
neg10 =
  mkFakeParameterChangeContext @[Integer]
    [(19, [])]

-- test_neg: neg11 - list param too deeply nested
neg11 :: ScriptContext
neg11 =
  mkFakeParameterChangeContext
    [
      ( 19
      ,
        [
          [ rat 2_000 10_000
          , rat 500 10_000_000
          ]
        ]
      )
    ]

-- test_neg: neg12 - non ParameterChange/TreasuryWithdrawals gov action
neg12 :: ScriptContext
neg12 = mkFakeContextFromGovAction InfoAction

-- test_unsorted1 - deliberately unsorted params (sorted validator rejects)
unsorted1 :: ScriptContext
unsorted1 =
  mkFakeParameterChangeContext @Integer
    [ (0, 30)
    , (17, 6_250)
    , (1, 10_000_001)
    ]

-- ============================================================================
-- Test runner
-- ============================================================================

mkConstitutionTests :: String -> Script -> TestTree
mkConstitutionTests name script =
  let eval config ctx =
        let (res, _budget, _logs) = evalScript (applyArguments script [PlutusTx.toData config, PlutusTx.toData ctx])
         in isRight res
      assertSucceeds config ctx =
        let (res, _budget, logs) = evalScript (applyArguments script [PlutusTx.toData config, PlutusTx.toData ctx])
         in assertBool ("expected success, got: " ++ show res ++ " logs: " ++ show logs) (isRight res)
      assertFails config ctx =
        assertBool "expected script failure" (not (eval config ctx))
   in testGroup
        name
        [ testGroup
            "Positive"
            [ testCase "pos1_multiple_integer_params_in_range" $ assertSucceeds constitutionConfig pos1
            , testCase "pos2_single_rational_param_in_range" $ assertSucceeds constitutionConfig pos2
            , testCase "pos3_list_param_execution_unit_prices" $ assertSucceeds constitutionConfig pos3
            , testCase "pos4_empty_params" $ assertSucceeds constitutionConfig pos4
            , testCase "pos5_treasury_withdrawal" $ assertSucceeds constitutionConfig pos5
            ]
        , testGroup
            "Negative"
            [ testCase "neg1_below_min_plus_unknown_param" $ assertFails constitutionConfig neg1
            , testCase "neg2_below_min" $ assertFails constitutionConfig neg2
            , testCase "neg3_unknown_negative_param" $ assertFails constitutionConfig neg3
            , testCase "neg4_type_mismatch_int_for_rational" $ assertFails constitutionConfig neg4
            , testCase "neg5_type_mismatch_rational_for_int" $ assertFails constitutionConfig neg5
            , testCase "neg6_rational_out_of_limits" $ assertFails constitutionConfig neg6
            , testCase "neg7_list_sub_param_out_of_range" $ assertFails constitutionConfig neg7
            , testCase "neg8_list_too_many_sub_params" $ assertFails constitutionConfig neg8
            , testCase "neg9_list_too_few_sub_params" $ assertFails constitutionConfig neg9
            , testCase "neg10_list_empty" $ assertFails constitutionConfig neg10
            , testCase "neg11_list_too_deeply_nested" $ assertFails constitutionConfig neg11
            , testCase "neg12_info_action_rejected" $ assertFails constitutionConfig neg12
            ]
        , testGroup
            "Sorted"
            [ testCase "unsorted1_unsorted_params_rejected" $ assertFails constitutionConfig unsorted1
            ]
        ]

-- ============================================================================
-- Benchmark scenarios
-- ============================================================================

constitutionBenchScenarios :: [(String, [(Integer, ParamValue)], ScriptContext)]
constitutionBenchScenarios =
  [ ("pos1: multi int params", constitutionConfig, pos1)
  , ("pos2: single rational", constitutionConfig, pos2)
  , ("pos3: list param", constitutionConfig, pos3)
  , ("pos4: empty params", constitutionConfig, pos4)
  , ("pos5: treasury withdrawal", constitutionConfig, pos5)
  , ("neg1: below min + unknown (reject)", constitutionConfig, neg1)
  , ("neg2: below min (reject)", constitutionConfig, neg2)
  , ("neg3: unknown param (reject)", constitutionConfig, neg3)
  , ("neg4: type mismatch int (reject)", constitutionConfig, neg4)
  , ("neg6: rational out of limits (reject)", constitutionConfig, neg6)
  , ("neg7: list sub-param OOR (reject)", constitutionConfig, neg7)
  , ("neg8: list too many (reject)", constitutionConfig, neg8)
  , ("neg12: info action (reject)", constitutionConfig, neg12)
  , ("unsorted1: unsorted (reject)", constitutionConfig, unsorted1)
  ]

-- ============================================================================
-- Conformance property test: both scripts must agree on all inputs
-- ============================================================================

evalScript' :: Script -> [(Integer, ParamValue)] -> ScriptContext -> Bool
evalScript' script config ctx =
  let (res, _, _) = evalScript (applyArguments script [PlutusTx.toData config, PlutusTx.toData ctx])
   in isRight res

genPredKey :: QC.Gen PredKey
genPredKey = QC.elements [MinValue, MaxValue, NotEqual]

genIntPreds :: QC.Gen [(PredKey, [Integer])]
genIntPreds = do
  n <- QC.chooseInt (0, 3)
  QC.vectorOf n $ do
    pk <- genPredKey
    vals <- QC.listOf1 (QC.chooseInteger (-100, 100))
    pure (pk, vals)

genRatPreds :: QC.Gen [(PredKey, [(Integer, Integer)])]
genRatPreds = do
  n <- QC.chooseInt (0, 2)
  QC.vectorOf n $ do
    pk <- genPredKey
    vals <- QC.listOf1 $ do
      num <- QC.chooseInteger (-10, 10)
      den <- QC.chooseInteger (1, 10)
      pure (num, den)
    pure (pk, vals)

genParamValue :: Int -> QC.Gen ParamValue
genParamValue depth =
  if depth <= 0
    then QC.oneof [ParamInteger <$> genIntPreds, ParamAny <$ QC.arbitrary @()]
    else
      QC.frequency
        [ (3, ParamInteger <$> genIntPreds)
        , (2, ParamRational <$> genRatPreds)
        ,
          ( 1
          , do
              n <- QC.chooseInt (1, 3)
              ParamList <$> QC.vectorOf n (genParamValue (depth - 1))
          )
        , (2, pure ParamAny)
        ]

genConfig :: QC.Gen [(Integer, ParamValue)]
genConfig = do
  n <- QC.chooseInt (0, 6)
  pids <- QC.vectorOf n (QC.chooseInteger (0, 20))
  let sortedUniquePids = nub (sort pids)
  mapM (\pid -> (pid,) <$> genParamValue 2) sortedUniquePids

genChangedIntParams :: [(Integer, ParamValue)] -> QC.Gen [(Integer, BuiltinData)]
genChangedIntParams cfg = do
  let intPids = [pid | (pid, ParamInteger _) <- cfg]
      ratPids = [pid | (pid, ParamRational _) <- cfg]
      anyPids = [pid | (pid, ParamAny) <- cfg]
      listPids = [pid | (pid, ParamList _) <- cfg]
  selectedInts <- QC.sublistOf intPids
  selectedRats <- QC.sublistOf ratPids
  selectedAnys <- QC.sublistOf anyPids
  selectedLists <- QC.sublistOf listPids
  unknowns <- do
    n <- QC.chooseInt (0, 1)
    QC.vectorOf n (QC.chooseInteger (50, 60))
  intEntries <-
    mapM
      (\pid -> do v <- QC.chooseInteger (-200, 200); pure (pid, PlutusTx.toBuiltinData v))
      selectedInts
  ratEntries <-
    mapM
      ( \pid -> do
          n <- QC.chooseInteger (-10, 10)
          d <- QC.chooseInteger (1, 10)
          pure (pid, PlutusTx.toBuiltinData [n, d :: Integer])
      )
      selectedRats
  let anyEntries = map (,PlutusTx.toBuiltinData (42 :: Integer)) selectedAnys
  let listEntries = map (,PlutusTx.toBuiltinData ([] :: [Integer])) selectedLists
  let unknownEntries = map (,PlutusTx.toBuiltinData (0 :: Integer)) unknowns
  pure $ sort (intEntries ++ ratEntries ++ anyEntries ++ listEntries ++ unknownEntries)

genScriptContext :: [(Integer, ParamValue)] -> QC.Gen ScriptContext
genScriptContext cfg = do
  useParameterChange <- QC.frequency [(8, pure True), (1, pure False)]
  if useParameterChange
    then do
      params <- genChangedIntParams cfg
      pure $ mkFakeParameterChangeContext params
    else
      pure $ mkFakeContextFromGovAction (TreasuryWithdrawals Map.empty Nothing)

mkConstitutionConformanceTests :: Script -> Script -> TestTree
mkConstitutionConformanceTests plutarchScript plinthScript =
  testGroup
    "Plutarch-Plinth Conformance"
    [ testProperty "prop_both_scripts_agree" $
        QC.forAll genConfig $ \config ->
          QC.forAll (genScriptContext config) $ \ctx ->
            let plutarchResult = evalScript' plutarchScript config ctx
                plinthResult = evalScript' plinthScript config ctx
             in plutarchResult QC.=== plinthResult
    ]
