{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Voting.Test.Voting (
  mkVotingTests,
  mkVotingConformanceTests,
  votingBenchScenarios,
) where

import Data.Either (isRight)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Script (Script)
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3
import PlutusTx qualified
import ProgrammableTokens.Test.ScriptContext.Builder (
  buildScriptContext,
  mkAdaValue,
  withAddress,
  withInput,
  withOutRef,
  withScriptInput,
  withValue,
  withVotingScript,
 )
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)

-- ============================================================================
-- Constants
-- ============================================================================

nftCS :: CurrencySymbol
nftCS = CurrencySymbol "aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd"

nftTN :: TokenName
nftTN = TokenName "HotNFT"

otherCS :: CurrencySymbol
otherCS = CurrencySymbol "11223344112233441122334411223344112233441122334411223344"

otherTN :: TokenName
otherTN = TokenName "OtherNFT"

someScriptHash :: ScriptHash
someScriptHash = ScriptHash "aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd"

someScriptAddr :: Address
someScriptAddr = Address (ScriptCredential someScriptHash) Nothing

somePkh :: PubKeyHash
somePkh = PubKeyHash "1122334411223344112233441122334411223344112233441122334455667788"

somePkhAddr :: Address
somePkhAddr = Address (PubKeyCredential somePkh) Nothing

someVoter :: Voter
someVoter = CommitteeVoter (HotCommitteeCredential (ScriptCredential someScriptHash))

nftValue :: Value
nftValue = mkAdaValue 5_000_000 <> assetClassValue (assetClass nftCS nftTN) 1

otherNFTValue :: Value
otherNFTValue = mkAdaValue 5_000_000 <> assetClassValue (assetClass otherCS otherTN) 1

adaOnlyValue :: Value
adaOnlyValue = mkAdaValue 10_000_000

-- ============================================================================
-- Context construction
-- ============================================================================

mkVotingContextWithInputs :: Voter -> [(Address, Value, TxOutRef)] -> ScriptContext
mkVotingContextWithInputs voter inputs =
  buildScriptContext
    ( withVotingScript (PlutusTx.toBuiltinData ()) voter
        <> mconcat
          [ case txOutAddress of
              Address (ScriptCredential _) _ ->
                withScriptInput
                  (PlutusTx.toBuiltinData ())
                  (withOutRef ref <> withAddress txOutAddress <> withValue val)
              _ ->
                withInput
                  (withOutRef ref <> withAddress txOutAddress <> withValue val)
          | (txOutAddress, val, ref) <- inputs
          ]
    )

-- ============================================================================
-- Positive test contexts
-- ============================================================================

posNFTInScriptInput :: ScriptContext
posNFTInScriptInput =
  mkVotingContextWithInputs someVoter
    [(someScriptAddr, nftValue, TxOutRef "aa00" 0)]

posNFTInPubKeyInput :: ScriptContext
posNFTInPubKeyInput =
  mkVotingContextWithInputs someVoter
    [(somePkhAddr, nftValue, TxOutRef "aa00" 0)]

posNFTAmongMultipleInputs :: ScriptContext
posNFTAmongMultipleInputs =
  mkVotingContextWithInputs someVoter
    [ (somePkhAddr, adaOnlyValue, TxOutRef "aa00" 0)
    , (somePkhAddr, nftValue, TxOutRef "bb00" 0)
    , (somePkhAddr, adaOnlyValue, TxOutRef "cc00" 0)
    ]

posNFTWithOtherTokens :: ScriptContext
posNFTWithOtherTokens =
  mkVotingContextWithInputs someVoter
    [(somePkhAddr, nftValue <> otherNFTValue, TxOutRef "aa00" 0)]

-- ============================================================================
-- Negative test contexts
-- ============================================================================

negNoNFTInInputs :: ScriptContext
negNoNFTInInputs =
  mkVotingContextWithInputs someVoter
    [(somePkhAddr, adaOnlyValue, TxOutRef "aa00" 0)]

negWrongCS :: ScriptContext
negWrongCS =
  mkVotingContextWithInputs someVoter
    [(somePkhAddr, otherNFTValue, TxOutRef "aa00" 0)]

negWrongTN :: ScriptContext
negWrongTN =
  mkVotingContextWithInputs someVoter
    [(somePkhAddr, mkAdaValue 5_000_000 <> assetClassValue (assetClass nftCS otherTN) 1, TxOutRef "aa00" 0)]

negEmptyInputs :: ScriptContext
negEmptyInputs =
  mkVotingContextWithInputs someVoter []

negMultipleInputsNoNFT :: ScriptContext
negMultipleInputsNoNFT =
  mkVotingContextWithInputs someVoter
    [ (somePkhAddr, adaOnlyValue, TxOutRef "aa00" 0)
    , (somePkhAddr, adaOnlyValue, TxOutRef "bb00" 0)
    , (somePkhAddr, otherNFTValue, TxOutRef "cc00" 0)
    ]

-- ============================================================================
-- Test runner
-- ============================================================================

mkVotingTests :: String -> Script -> TestTree
mkVotingTests name script =
  let eval cs tn ctx =
        let (res, _budget, _logs) = evalScript (applyArguments script [PlutusTx.toData cs, PlutusTx.toData tn, PlutusTx.toData ctx])
         in isRight res
      assertSucceeds cs tn ctx =
        let (res, _budget, logs) = evalScript (applyArguments script [PlutusTx.toData cs, PlutusTx.toData tn, PlutusTx.toData ctx])
         in assertBool ("expected success, got: " ++ show res ++ " logs: " ++ show logs) (isRight res)
      assertFails cs tn ctx =
        assertBool "expected script failure" (not (eval cs tn ctx))
   in testGroup
        name
        [ testGroup
            "Positive"
            [ testCase "nft_in_script_input" $ assertSucceeds nftCS nftTN posNFTInScriptInput
            , testCase "nft_in_pubkey_input" $ assertSucceeds nftCS nftTN posNFTInPubKeyInput
            , testCase "nft_among_multiple_inputs" $ assertSucceeds nftCS nftTN posNFTAmongMultipleInputs
            , testCase "nft_with_other_tokens" $ assertSucceeds nftCS nftTN posNFTWithOtherTokens
            ]
        , testGroup
            "Negative"
            [ testCase "no_nft_in_inputs" $ assertFails nftCS nftTN negNoNFTInInputs
            , testCase "wrong_currency_symbol" $ assertFails nftCS nftTN negWrongCS
            , testCase "wrong_token_name" $ assertFails nftCS nftTN negWrongTN
            , testCase "empty_inputs" $ assertFails nftCS nftTN negEmptyInputs
            , testCase "multiple_inputs_no_nft" $ assertFails nftCS nftTN negMultipleInputsNoNFT
            ]
        ]

-- ============================================================================
-- Benchmark scenarios
-- ============================================================================

votingBenchScenarios :: [(String, CurrencySymbol, TokenName, ScriptContext)]
votingBenchScenarios =
  [ ("nft in script input", nftCS, nftTN, posNFTInScriptInput)
  , ("nft in pubkey input", nftCS, nftTN, posNFTInPubKeyInput)
  , ("nft among multiple inputs", nftCS, nftTN, posNFTAmongMultipleInputs)
  , ("nft with other tokens", nftCS, nftTN, posNFTWithOtherTokens)
  , ("no nft (reject)", nftCS, nftTN, negNoNFTInInputs)
  , ("wrong CS (reject)", nftCS, nftTN, negWrongCS)
  , ("wrong TN (reject)", nftCS, nftTN, negWrongTN)
  , ("empty inputs (reject)", nftCS, nftTN, negEmptyInputs)
  ]

-- ============================================================================
-- Conformance property test
-- ============================================================================

evalScript' :: Script -> CurrencySymbol -> TokenName -> ScriptContext -> Bool
evalScript' script cs tn ctx =
  let (res, _, _) = evalScript (applyArguments script [PlutusTx.toData cs, PlutusTx.toData tn, PlutusTx.toData ctx])
   in isRight res

genCurrencySymbol :: QC.Gen CurrencySymbol
genCurrencySymbol =
  QC.elements [nftCS, otherCS, CurrencySymbol ""]

genTokenName :: QC.Gen TokenName
genTokenName =
  QC.elements [nftTN, otherTN, TokenName ""]

genInputValue :: CurrencySymbol -> TokenName -> QC.Gen Value
genInputValue cs tn = do
  hasNFT <- QC.arbitrary
  hasOther <- QC.arbitrary
  let base = mkAdaValue 5_000_000
      nft = if hasNFT then assetClassValue (assetClass cs tn) 1 else mempty
      other = if hasOther then assetClassValue (assetClass otherCS otherTN) 1 else mempty
  pure $ base <> nft <> other

genInputs :: CurrencySymbol -> TokenName -> QC.Gen [(Address, Value, TxOutRef)]
genInputs cs tn = do
  n <- QC.chooseInt (0, 5)
  mapM
    ( \i -> do
        val <- genInputValue cs tn
        pure (somePkhAddr, val, TxOutRef "ff00" (fromIntegral i))
    )
    [0 .. n - 1]

mkVotingConformanceTests :: Script -> Script -> TestTree
mkVotingConformanceTests plutarchScript plinthScript =
  testGroup
    "Plutarch-Plinth Conformance"
    [ testProperty "prop_both_scripts_agree" $
        QC.forAll genCurrencySymbol $ \searchCS ->
          QC.forAll genTokenName $ \searchTN ->
            QC.forAll (genInputs nftCS nftTN) $ \inputs ->
              let ctx = mkVotingContextWithInputs someVoter inputs
                  plutarchResult = evalScript' plutarchScript searchCS searchTN ctx
                  plinthResult = evalScript' plinthScript searchCS searchTN ctx
               in plutarchResult QC.=== plinthResult
    ]
