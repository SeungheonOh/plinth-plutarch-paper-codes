{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hydra.Test.Head (
  mkHeadTests,
  mkHeadConformanceTests,
  headBenchScenarios,
) where

import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.Word (Word8)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Script (Script)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import ProgrammableTokens.Test.ScriptContext.Builder
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Hydra.Types.HeadState

-- ============================================================================
-- Constants
-- ============================================================================

bs28 :: Word8 -> BuiltinByteString
bs28 w = PV1.toBuiltin (BS.replicate 28 w)

bs32 :: Word8 -> BuiltinByteString
bs32 w = PV1.toBuiltin (BS.replicate 32 w)

headCS :: CurrencySymbol
headCS = CurrencySymbol (bs28 0x20)

headScriptHash :: ScriptHash
headScriptHash = ScriptHash (bs28 0x21)

headAddress :: Address
headAddress = Address (ScriptCredential headScriptHash) Nothing

signerPkh :: PubKeyHash
signerPkh = PubKeyHash (bs28 0x01)

signerPkh2 :: PubKeyHash
signerPkh2 = PubKeyHash (bs28 0x02)

hydraHeadV2 :: BuiltinByteString
hydraHeadV2 = "HydraHeadV2"

headUtxoRef :: TxOutRef
headUtxoRef = TxOutRef (TxId (bs28 0xAA)) 0

seedRef :: TxOutRef
seedRef = TxOutRef (TxId (bs28 0xBB)) 0

pubKeyAddress :: PubKeyHash -> Address
pubKeyAddress pkh = Address (PubKeyCredential pkh) Nothing

mkValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
mkValue = foldMap (\(cs, tn, amount) -> assetClassValue (assetClass cs tn) amount)

-- ============================================================================
-- Hashing helpers (off-chain)
-- ============================================================================

hashTxOuts :: [TxOut] -> BuiltinByteString
hashTxOuts = Builtins.sha2_256 . foldMap (Builtins.serialiseData . PlutusTx.toBuiltinData)

emptyHash :: BuiltinByteString
emptyHash = hashTxOuts []

-- ============================================================================
-- Head value with state token + participation tokens
-- ============================================================================

headValueWithTokens :: [PubKeyHash] -> Value
headValueWithTokens pkhList =
  mkAdaValue 10_000_000
    <> mkValue [(headCS, TokenName hydraHeadV2, 1)]
    <> foldMap (\pkh -> mkValue [(headCS, TokenName (getPubKeyHash pkh), 1)]) pkhList

burnHeadTokens :: [PubKeyHash] -> Value
burnHeadTokens pkhList =
  mkValue [(headCS, TokenName hydraHeadV2, -1)]
    <> foldMap (\pkh -> mkValue [(headCS, TokenName (getPubKeyHash pkh), -1)]) pkhList

-- ============================================================================
-- Parties (as BuiltinByteString vkeys)
-- ============================================================================

partyVkeys :: [BuiltinByteString]
partyVkeys = [getPubKeyHash signerPkh]

partyVkeys2 :: [BuiltinByteString]
partyVkeys2 = [getPubKeyHash signerPkh, getPubKeyHash signerPkh2]

-- ============================================================================
-- Fanout test context builders
-- ============================================================================

fanoutOutput :: TxOut
fanoutOutput =
  TxOut
    { txOutAddress = pubKeyAddress signerPkh
    , txOutValue = mkAdaValue 5_000_000
    , txOutDatum = NoOutputDatum
    , txOutReferenceScript = Nothing
    }

fanoutUtxoHash :: BuiltinByteString
fanoutUtxoHash = hashTxOuts [fanoutOutput]

mkClosedDatumForFanout :: Integer -> ClosedDatum
mkClosedDatumForFanout deadline =
  ClosedDatum
    { closedHeadId = headCS
    , closedParties = partyVkeys
    , closedContestationPeriod = 200
    , closedVersion = 1
    , closedSnapshotNumber = 1
    , closedUtxoHash = fanoutUtxoHash
    , closedAlphaUTxOHash = emptyHash
    , closedOmegaUTxOHash = emptyHash
    , closedContesters = [signerPkh]
    , closedContestationDeadline = POSIXTime deadline
    }

mkFanoutCtx :: ScriptContext
mkFanoutCtx =
  let closedDatum = mkClosedDatumForFanout 1000
      state = Closed closedDatum
      fanoutRedeemer = Fanout{fanoutNumberOfFanoutOutputs = 1, fanoutNumberOfCommitOutputs = 0, fanoutNumberOfDecommitOutputs = 0}
   in buildBalancedScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData fanoutRedeemer)
            ( withOutRef headUtxoRef
                <> withAddress headAddress
                <> withValue (headValueWithTokens [signerPkh])
                <> withInlineDatum (PlutusTx.toBuiltinData state)
            )
            <> withSigner signerPkh
            <> withOutput
              ( withTxOutAddress (pubKeyAddress signerPkh)
                  <> withTxOutValue (mkAdaValue 5_000_000)
              )
            <> withMint (burnHeadTokens [signerPkh]) (PlutusTx.toBuiltinData ())
            <> withValidRange
              ( Interval
                  (LowerBound (Finite 1001) True)
                  (UpperBound PosInf True)
              )
        )

mkFanoutHashMismatchCtx :: ScriptContext
mkFanoutHashMismatchCtx =
  let closedDatum = mkClosedDatumForFanout 1000
      state = Closed closedDatum
      fanoutRedeemer = Fanout{fanoutNumberOfFanoutOutputs = 1, fanoutNumberOfCommitOutputs = 0, fanoutNumberOfDecommitOutputs = 0}
   in buildBalancedScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData fanoutRedeemer)
            ( withOutRef headUtxoRef
                <> withAddress headAddress
                <> withValue (headValueWithTokens [signerPkh])
                <> withInlineDatum (PlutusTx.toBuiltinData state)
            )
            <> withSigner signerPkh
            <> withOutput
              ( withTxOutAddress (pubKeyAddress signerPkh)
                  <> withTxOutValue (mkAdaValue 99_000_000)
              )
            <> withMint (burnHeadTokens [signerPkh]) (PlutusTx.toBuiltinData ())
            <> withValidRange
              ( Interval
                  (LowerBound (Finite 1001) True)
                  (UpperBound PosInf True)
              )
        )

mkFanoutBeforeDeadlineCtx :: ScriptContext
mkFanoutBeforeDeadlineCtx =
  let closedDatum = mkClosedDatumForFanout 1000
      state = Closed closedDatum
      fanoutRedeemer = Fanout{fanoutNumberOfFanoutOutputs = 1, fanoutNumberOfCommitOutputs = 0, fanoutNumberOfDecommitOutputs = 0}
   in buildBalancedScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData fanoutRedeemer)
            ( withOutRef headUtxoRef
                <> withAddress headAddress
                <> withValue (headValueWithTokens [signerPkh])
                <> withInlineDatum (PlutusTx.toBuiltinData state)
            )
            <> withSigner signerPkh
            <> withOutput
              ( withTxOutAddress (pubKeyAddress signerPkh)
                  <> withTxOutValue (mkAdaValue 5_000_000)
              )
            <> withMint (burnHeadTokens [signerPkh]) (PlutusTx.toBuiltinData ())
            <> withValidRange
              ( Interval
                  (LowerBound (Finite 999) True)
                  (UpperBound PosInf True)
              )
        )

mkFanoutBurnMismatchCtx :: ScriptContext
mkFanoutBurnMismatchCtx =
  let closedDatum = mkClosedDatumForFanout 1000
      state = Closed closedDatum
      fanoutRedeemer = Fanout{fanoutNumberOfFanoutOutputs = 1, fanoutNumberOfCommitOutputs = 0, fanoutNumberOfDecommitOutputs = 0}
   in buildBalancedScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData fanoutRedeemer)
            ( withOutRef headUtxoRef
                <> withAddress headAddress
                <> withValue (headValueWithTokens [signerPkh])
                <> withInlineDatum (PlutusTx.toBuiltinData state)
            )
            <> withSigner signerPkh
            <> withOutput
              ( withTxOutAddress (pubKeyAddress signerPkh)
                  <> withTxOutValue (mkAdaValue 5_000_000)
              )
            <> withMint (mkValue [(headCS, TokenName hydraHeadV2, -1)]) (PlutusTx.toBuiltinData ())
            <> withValidRange
              ( Interval
                  (LowerBound (Finite 1001) True)
                  (UpperBound PosInf True)
              )
        )

-- ============================================================================
-- Close Initial test context builders
-- ============================================================================

mkOpenDatumForClose :: OpenDatum
mkOpenDatumForClose =
  OpenDatum
    { openHeadSeed = seedRef
    , openHeadId = headCS
    , openParties = partyVkeys
    , openContestationPeriod = 200
    , openVersion = 0
    , openUtxoHash = emptyHash
    }

mkCloseInitialCtx :: ScriptContext
mkCloseInitialCtx =
  let openDatum = mkOpenDatumForClose
      state = Open openDatum
      closeRedeemer = Close CloseInitial
      closedOutputDatum =
        Closed $
          ClosedDatum
            { closedHeadId = headCS
            , closedParties = partyVkeys
            , closedContestationPeriod = 200
            , closedVersion = 0
            , closedSnapshotNumber = 0
            , closedUtxoHash = emptyHash
            , closedAlphaUTxOHash = emptyHash
            , closedOmegaUTxOHash = emptyHash
            , closedContesters = []
            , closedContestationDeadline = POSIXTime 400
            }
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData closeRedeemer)
            ( withOutRef headUtxoRef
                <> withAddress headAddress
                <> withValue (headValueWithTokens [signerPkh])
                <> withInlineDatum (PlutusTx.toBuiltinData state)
            )
            <> withSigner signerPkh
            <> withOutput
              ( withTxOutAddress headAddress
                  <> withTxOutValue (headValueWithTokens [signerPkh])
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData closedOutputDatum)
              )
            <> withValidRange
              ( Interval
                  (LowerBound (Finite 100) True)
                  (UpperBound (Finite 200) True)
              )
        )

mkCloseInitialWrongVersionCtx :: ScriptContext
mkCloseInitialWrongVersionCtx =
  let openDatum = mkOpenDatumForClose{openVersion = 1}
      state = Open openDatum
      closeRedeemer = Close CloseInitial
      closedOutputDatum =
        Closed $
          ClosedDatum
            { closedHeadId = headCS
            , closedParties = partyVkeys
            , closedContestationPeriod = 200
            , closedVersion = 1
            , closedSnapshotNumber = 0
            , closedUtxoHash = emptyHash
            , closedAlphaUTxOHash = emptyHash
            , closedOmegaUTxOHash = emptyHash
            , closedContesters = []
            , closedContestationDeadline = POSIXTime 400
            }
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData closeRedeemer)
            ( withOutRef headUtxoRef
                <> withAddress headAddress
                <> withValue (headValueWithTokens [signerPkh])
                <> withInlineDatum (PlutusTx.toBuiltinData state)
            )
            <> withSigner signerPkh
            <> withOutput
              ( withTxOutAddress headAddress
                  <> withTxOutValue (headValueWithTokens [signerPkh])
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData closedOutputDatum)
              )
            <> withValidRange
              ( Interval
                  (LowerBound (Finite 100) True)
                  (UpperBound (Finite 200) True)
              )
        )

-- ============================================================================
-- Invalid transition test
-- ============================================================================

mkInvalidTransitionCtx :: ScriptContext
mkInvalidTransitionCtx =
  let closedDatum = mkClosedDatumForFanout 1000
      state = Closed closedDatum
      incrementRedeemer = Increment $ IncrementRedeemer [] 0 (TxOutRef "" 0)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData incrementRedeemer)
            ( withOutRef headUtxoRef
                <> withAddress headAddress
                <> withValue (headValueWithTokens [signerPkh])
                <> withInlineDatum (PlutusTx.toBuiltinData state)
            )
        )

-- ============================================================================
-- Contest test context builders
-- ============================================================================

mkClosedDatumForContest :: ClosedDatum
mkClosedDatumForContest =
  ClosedDatum
    { closedHeadId = headCS
    , closedParties = partyVkeys
    , closedContestationPeriod = 200
    , closedVersion = 1
    , closedSnapshotNumber = 1
    , closedUtxoHash = emptyHash
    , closedAlphaUTxOHash = emptyHash
    , closedOmegaUTxOHash = emptyHash
    , closedContesters = []
    , closedContestationDeadline = POSIXTime 1000
    }

mkContestCtx :: ScriptContext
mkContestCtx =
  let closedDatum = mkClosedDatumForContest
      state = Closed closedDatum
      contestRedeemer = Contest $ ContestCurrent{contestCurrentSig = []}
      contestedOutputDatum =
        Closed $
          ClosedDatum
            { closedHeadId = headCS
            , closedParties = partyVkeys
            , closedContestationPeriod = 200
            , closedVersion = 1
            , closedSnapshotNumber = 2
            , closedUtxoHash = emptyHash
            , closedAlphaUTxOHash = emptyHash
            , closedOmegaUTxOHash = emptyHash
            , closedContesters = [signerPkh]
            , closedContestationDeadline = POSIXTime 1200
            }
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData contestRedeemer)
            ( withOutRef headUtxoRef
                <> withAddress headAddress
                <> withValue (headValueWithTokens [signerPkh])
                <> withInlineDatum (PlutusTx.toBuiltinData state)
            )
            <> withSigner signerPkh
            <> withOutput
              ( withTxOutAddress headAddress
                  <> withTxOutValue (headValueWithTokens [signerPkh])
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData contestedOutputDatum)
              )
            <> withValidRange
              ( Interval
                  (LowerBound (Finite 500) True)
                  (UpperBound (Finite 900) True)
              )
        )

mkContestNotNewerCtx :: ScriptContext
mkContestNotNewerCtx =
  let closedDatum = mkClosedDatumForContest
      state = Closed closedDatum
      contestRedeemer = Contest $ ContestCurrent{contestCurrentSig = []}
      contestedOutputDatum =
        Closed $
          ClosedDatum
            { closedHeadId = headCS
            , closedParties = partyVkeys
            , closedContestationPeriod = 200
            , closedVersion = 1
            , closedSnapshotNumber = 1
            , closedUtxoHash = emptyHash
            , closedAlphaUTxOHash = emptyHash
            , closedOmegaUTxOHash = emptyHash
            , closedContesters = [signerPkh]
            , closedContestationDeadline = POSIXTime 1200
            }
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData contestRedeemer)
            ( withOutRef headUtxoRef
                <> withAddress headAddress
                <> withValue (headValueWithTokens [signerPkh])
                <> withInlineDatum (PlutusTx.toBuiltinData state)
            )
            <> withSigner signerPkh
            <> withOutput
              ( withTxOutAddress headAddress
                  <> withTxOutValue (headValueWithTokens [signerPkh])
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData contestedOutputDatum)
              )
            <> withValidRange
              ( Interval
                  (LowerBound (Finite 500) True)
                  (UpperBound (Finite 900) True)
              )
        )

mkContestAfterDeadlineCtx :: ScriptContext
mkContestAfterDeadlineCtx =
  let closedDatum = mkClosedDatumForContest
      state = Closed closedDatum
      contestRedeemer = Contest $ ContestCurrent{contestCurrentSig = []}
      contestedOutputDatum =
        Closed $
          ClosedDatum
            { closedHeadId = headCS
            , closedParties = partyVkeys
            , closedContestationPeriod = 200
            , closedVersion = 1
            , closedSnapshotNumber = 2
            , closedUtxoHash = emptyHash
            , closedAlphaUTxOHash = emptyHash
            , closedOmegaUTxOHash = emptyHash
            , closedContesters = [signerPkh]
            , closedContestationDeadline = POSIXTime 1200
            }
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData contestRedeemer)
            ( withOutRef headUtxoRef
                <> withAddress headAddress
                <> withValue (headValueWithTokens [signerPkh])
                <> withInlineDatum (PlutusTx.toBuiltinData state)
            )
            <> withSigner signerPkh
            <> withOutput
              ( withTxOutAddress headAddress
                  <> withTxOutValue (headValueWithTokens [signerPkh])
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData contestedOutputDatum)
              )
            <> withValidRange
              ( Interval
                  (LowerBound (Finite 500) True)
                  (UpperBound (Finite 1100) True)
              )
        )

-- ============================================================================
-- Test runner
-- ============================================================================

mkHeadTests :: String -> Script -> TestTree
mkHeadTests name script =
  let eval ctx =
        let (res, _budget, _logs) = evalScript (applyArguments script [PlutusTx.toData ctx])
         in isRight res
      assertSucceeds ctx =
        let (res, _budget, logs) = evalScript (applyArguments script [PlutusTx.toData ctx])
         in assertBool ("expected success, got: " ++ show res ++ " logs: " ++ show logs) (isRight res)
      assertFails ctx =
        assertBool "expected script failure" (not (eval ctx))
   in testGroup
        name
        [ testCase "unit_fanout_valid" $ assertSucceeds mkFanoutCtx
        , testCase "unit_fanout_hash_mismatch_rejected" $ assertFails mkFanoutHashMismatchCtx
        , testCase "unit_fanout_before_deadline_rejected" $ assertFails mkFanoutBeforeDeadlineCtx
        , testCase "unit_fanout_burn_mismatch_rejected" $ assertFails mkFanoutBurnMismatchCtx
        , testCase "unit_close_initial_valid" $ assertSucceeds mkCloseInitialCtx
        , testCase "unit_close_initial_wrong_version_rejected" $ assertFails mkCloseInitialWrongVersionCtx
        , testCase "unit_contest_no_sigs_rejected" $ assertFails mkContestCtx
        , testCase "unit_contest_not_newer_rejected" $ assertFails mkContestNotNewerCtx
        , testCase "unit_contest_after_deadline_rejected" $ assertFails mkContestAfterDeadlineCtx
        , testCase "unit_invalid_transition_rejected" $ assertFails mkInvalidTransitionCtx
        ]

-- ============================================================================
-- Conformance: both scripts must agree on all scenarios
-- ============================================================================

mkHeadConformanceTests :: Script -> Script -> TestTree
mkHeadConformanceTests plutarchScript plinthScript =
  let allContexts =
        [ ("fanout_valid", mkFanoutCtx)
        , ("fanout_hash_mismatch", mkFanoutHashMismatchCtx)
        , ("fanout_before_deadline", mkFanoutBeforeDeadlineCtx)
        , ("fanout_burn_mismatch", mkFanoutBurnMismatchCtx)
        , ("close_initial_valid", mkCloseInitialCtx)
        , ("close_initial_wrong_version", mkCloseInitialWrongVersionCtx)
        , ("contest_valid", mkContestCtx)
        , ("contest_not_newer", mkContestNotNewerCtx)
        , ("contest_after_deadline", mkContestAfterDeadlineCtx)
        , ("invalid_transition", mkInvalidTransitionCtx)
        ]
      evalWith script ctx =
        let (res, _, _) = evalScript (applyArguments script [PlutusTx.toData ctx])
         in isRight res
      checkConformance (name, ctx) =
        testCase ("conformance_" ++ name) $
          assertBool
            ("Plutarch and Plinth disagree on " ++ name)
            (evalWith plutarchScript ctx == evalWith plinthScript ctx)
   in testGroup "Plutarch-Plinth Conformance" (map checkConformance allContexts)

-- ============================================================================
-- Benchmark scenarios (exported for BenchScripts)
-- ============================================================================

headBenchScenarios :: [(String, ScriptContext)]
headBenchScenarios =
  [ ("fanout: valid", mkFanoutCtx)
  , ("fanout: hash mismatch (reject)", mkFanoutHashMismatchCtx)
  , ("fanout: before deadline (reject)", mkFanoutBeforeDeadlineCtx)
  , ("fanout: burn mismatch (reject)", mkFanoutBurnMismatchCtx)
  , ("close initial: valid", mkCloseInitialCtx)
  , ("close initial: wrong version (reject)", mkCloseInitialWrongVersionCtx)
  , ("contest: no sigs (reject)", mkContestCtx)
  , ("contest: not newer (reject)", mkContestNotNewerCtx)
  , ("contest: after deadline (reject)", mkContestAfterDeadlineCtx)
  , ("invalid transition (reject)", mkInvalidTransitionCtx)
  ]
