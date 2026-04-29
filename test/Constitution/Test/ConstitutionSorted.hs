{-# LANGUAGE OverloadedStrings #-}

module Constitution.Test.ConstitutionSorted (
  mkConstitutionTests,
  constitutionBenchScenarios,
) where

import Data.Either (isRight)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Script (Script)
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import ProgrammableTokens.Test.ScriptContext.Builder (
  ScriptContextBuilder,
  buildScriptContext,
  withProposingScript,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Constitution.Types.ConstitutionConfig

-- ============================================================================
-- Constants
-- ============================================================================

dummyReturnAddr :: Credential
dummyReturnAddr = PubKeyCredential (PubKeyHash "")

-- ============================================================================
-- Config construction
-- ============================================================================

mkChangedParams :: [(Integer, BuiltinData)] -> ChangedParameters
mkChangedParams pairs = ChangedParameters (PlutusTx.toBuiltinData (Map.unsafeFromList pairs))

mkProposal :: ChangedParameters -> ProposalProcedure
mkProposal cp =
  ProposalProcedure
    { ppDeposit = 0
    , ppReturnAddr = dummyReturnAddr
    , ppGovernanceAction = ParameterChange Nothing cp Nothing
    }

mkTreasuryProposal :: ProposalProcedure
mkTreasuryProposal =
  ProposalProcedure
    { ppDeposit = 0
    , ppReturnAddr = dummyReturnAddr
    , ppGovernanceAction = TreasuryWithdrawals Map.empty Nothing
    }

mkCtx :: ProposalProcedure -> ScriptContext
mkCtx proposal =
  buildScriptContext
    ( withProposingScript
        (PlutusTx.toBuiltinData ())
        proposal
    )

-- ============================================================================
-- Test configs
-- ============================================================================

simpleConfig :: [(Integer, ParamValue)]
simpleConfig =
  [ (0, ParamInteger [(MinValue, [0]), (MaxValue, [100])])
  , (5, ParamInteger [(MinValue, [10])])
  , (10, ParamAny)
  ]

notEqualConfig :: [(Integer, ParamValue)]
notEqualConfig =
  [ (0, ParamInteger [(NotEqual, [0])])
  ]

rationalConfig :: [(Integer, ParamValue)]
rationalConfig =
  [ (0, ParamRational [(MinValue, [(1, 4)]), (MaxValue, [(3, 4)])])
  ]

listConfig :: [(Integer, ParamValue)]
listConfig =
  [ (0, ParamList [ParamInteger [(MinValue, [0])], ParamInteger [(MaxValue, [100])]])
  ]

-- ============================================================================
-- Test contexts
-- ============================================================================

validSingleParamCtx :: ScriptContext
validSingleParamCtx =
  mkCtx $ mkProposal $ mkChangedParams [(0, PlutusTx.toBuiltinData (50 :: Integer))]

validMultiParamCtx :: ScriptContext
validMultiParamCtx =
  mkCtx $
    mkProposal $
      mkChangedParams
        [ (0, PlutusTx.toBuiltinData (50 :: Integer))
        , (5, PlutusTx.toBuiltinData (20 :: Integer))
        ]

invalidTooHighCtx :: ScriptContext
invalidTooHighCtx =
  mkCtx $ mkProposal $ mkChangedParams [(0, PlutusTx.toBuiltinData (200 :: Integer))]

invalidTooLowCtx :: ScriptContext
invalidTooLowCtx =
  mkCtx $ mkProposal $ mkChangedParams [(5, PlutusTx.toBuiltinData (5 :: Integer))]

unknownParamCtx :: ScriptContext
unknownParamCtx =
  mkCtx $ mkProposal $ mkChangedParams [(3, PlutusTx.toBuiltinData (42 :: Integer))]

emptyProposalCtx :: ScriptContext
emptyProposalCtx =
  mkCtx $ mkProposal $ mkChangedParams []

treasuryCtx :: ScriptContext
treasuryCtx = mkCtx mkTreasuryProposal

paramAnyCtx :: ScriptContext
paramAnyCtx =
  mkCtx $ mkProposal $ mkChangedParams [(10, PlutusTx.toBuiltinData ("anything" :: BuiltinByteString))]

notEqualValidCtx :: ScriptContext
notEqualValidCtx =
  mkCtx $ mkProposal $ mkChangedParams [(0, PlutusTx.toBuiltinData (42 :: Integer))]

notEqualInvalidCtx :: ScriptContext
notEqualInvalidCtx =
  mkCtx $ mkProposal $ mkChangedParams [(0, PlutusTx.toBuiltinData (0 :: Integer))]

rationalValidCtx :: ScriptContext
rationalValidCtx =
  mkCtx $ mkProposal $ mkChangedParams [(0, PlutusTx.toBuiltinData (1 :: Integer, 2 :: Integer))]

rationalTooLowCtx :: ScriptContext
rationalTooLowCtx =
  mkCtx $ mkProposal $ mkChangedParams [(0, PlutusTx.toBuiltinData (1 :: Integer, 8 :: Integer))]

rationalTooHighCtx :: ScriptContext
rationalTooHighCtx =
  mkCtx $ mkProposal $ mkChangedParams [(0, PlutusTx.toBuiltinData (7 :: Integer, 8 :: Integer))]

listValidCtx :: ScriptContext
listValidCtx =
  mkCtx $
    mkProposal $
      mkChangedParams
        [ (0, PlutusTx.toBuiltinData [PlutusTx.toBuiltinData (5 :: Integer), PlutusTx.toBuiltinData (50 :: Integer)])
        ]

listInvalidCtx :: ScriptContext
listInvalidCtx =
  mkCtx $
    mkProposal $
      mkChangedParams
        [ (0, PlutusTx.toBuiltinData [PlutusTx.toBuiltinData (5 :: Integer), PlutusTx.toBuiltinData (200 :: Integer)])
        ]

skipConfigEntryCtx :: ScriptContext
skipConfigEntryCtx =
  mkCtx $ mkProposal $ mkChangedParams [(5, PlutusTx.toBuiltinData (50 :: Integer))]

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
        [ testCase "unit_valid_single_param" $ assertSucceeds simpleConfig validSingleParamCtx
        , testCase "unit_valid_multi_param" $ assertSucceeds simpleConfig validMultiParamCtx
        , testCase "unit_invalid_too_high" $ assertFails simpleConfig invalidTooHighCtx
        , testCase "unit_invalid_too_low" $ assertFails simpleConfig invalidTooLowCtx
        , testCase "unit_unknown_param_rejected" $ assertFails simpleConfig unknownParamCtx
        , testCase "unit_empty_proposal_succeeds" $ assertSucceeds simpleConfig emptyProposalCtx
        , testCase "unit_treasury_withdrawal_succeeds" $ assertSucceeds simpleConfig treasuryCtx
        , testCase "unit_param_any_succeeds" $ assertSucceeds simpleConfig paramAnyCtx
        , testCase "unit_not_equal_valid" $ assertSucceeds notEqualConfig notEqualValidCtx
        , testCase "unit_not_equal_invalid" $ assertFails notEqualConfig notEqualInvalidCtx
        , testCase "unit_rational_valid" $ assertSucceeds rationalConfig rationalValidCtx
        , testCase "unit_rational_too_low" $ assertFails rationalConfig rationalTooLowCtx
        , testCase "unit_rational_too_high" $ assertFails rationalConfig rationalTooHighCtx
        , testCase "unit_list_valid" $ assertSucceeds listConfig listValidCtx
        , testCase "unit_list_invalid" $ assertFails listConfig listInvalidCtx
        , testCase "unit_skip_config_entry" $ assertSucceeds simpleConfig skipConfigEntryCtx
        ]

-- ============================================================================
-- Benchmark scenarios
-- ============================================================================

constitutionBenchScenarios :: [(String, [(Integer, ParamValue)], ScriptContext)]
constitutionBenchScenarios =
  [ ("valid: single param", simpleConfig, validSingleParamCtx)
  , ("valid: multi param", simpleConfig, validMultiParamCtx)
  , ("invalid: too high (reject)", simpleConfig, invalidTooHighCtx)
  , ("invalid: too low (reject)", simpleConfig, invalidTooLowCtx)
  , ("unknown param (reject)", simpleConfig, unknownParamCtx)
  , ("empty proposal", simpleConfig, emptyProposalCtx)
  , ("treasury withdrawal", simpleConfig, treasuryCtx)
  , ("param any", simpleConfig, paramAnyCtx)
  , ("not equal: valid", notEqualConfig, notEqualValidCtx)
  , ("not equal: invalid (reject)", notEqualConfig, notEqualInvalidCtx)
  , ("rational: valid", rationalConfig, rationalValidCtx)
  , ("rational: too low (reject)", rationalConfig, rationalTooLowCtx)
  , ("rational: too high (reject)", rationalConfig, rationalTooHighCtx)
  , ("list: valid", listConfig, listValidCtx)
  , ("list: invalid (reject)", listConfig, listInvalidCtx)
  , ("skip config entry", simpleConfig, skipConfigEntryCtx)
  ]
