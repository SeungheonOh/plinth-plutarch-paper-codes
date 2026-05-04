{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Crowdfund.Test.Crowdfund (
  mkCrowdfundTests,
  mkCrowdfundConformanceTests,
  crowdfundBenchScenarios,
) where

import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.Word (Word8)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Script (Script)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import ProgrammableTokens.Test.ScriptContext.Builder
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Crowdfund.Types.CrowdfundState

-- ============================================================================
-- Constants
-- ============================================================================

bs28 :: Word8 -> BuiltinByteString
bs28 w = PV1.toBuiltin (BS.replicate 28 w)

contractScriptHash :: ScriptHash
contractScriptHash = ScriptHash (bs28 0x30)

contractAddress :: Address
contractAddress = Address (ScriptCredential contractScriptHash) Nothing

recipientPkh :: PubKeyHash
recipientPkh = PubKeyHash (bs28 0x01)

donor1Pkh :: PubKeyHash
donor1Pkh = PubKeyHash (bs28 0x02)

donor2Pkh :: PubKeyHash
donor2Pkh = PubKeyHash (bs28 0x03)

donor3Pkh :: PubKeyHash
donor3Pkh = PubKeyHash (bs28 0x04)

contractUtxoRef :: TxOutRef
contractUtxoRef = TxOutRef (TxId (bs28 0xAA)) 0

pubKeyAddress :: PubKeyHash -> Address
pubKeyAddress pkh = Address (PubKeyCredential pkh) Nothing

defaultDeadline :: POSIXTime
defaultDeadline = POSIXTime 1000

defaultGoal :: Integer
defaultGoal = 10_000_000

beforeDeadline :: POSIXTimeRange
beforeDeadline =
  Interval
    (LowerBound (Finite 500) True)
    (UpperBound (Finite 999) True)

afterDeadline :: POSIXTimeRange
afterDeadline =
  Interval
    (LowerBound (Finite 1001) True)
    (UpperBound (Finite 2000) True)

-- ============================================================================
-- Datum helpers
-- ============================================================================

mkDatum :: Map.Map PubKeyHash Integer -> CrowdfundDatum
mkDatum wallets =
  CrowdfundDatum
    { cfRecipient = recipientPkh
    , cfGoal = defaultGoal
    , cfDeadline = defaultDeadline
    , cfWallets = wallets
    }

emptyWallets :: Map.Map PubKeyHash Integer
emptyWallets = Map.empty

singleWallet :: PubKeyHash -> Integer -> Map.Map PubKeyHash Integer
singleWallet k v = Map.unsafeFromList [(k, v)]

twoWallets :: PubKeyHash -> Integer -> PubKeyHash -> Integer -> Map.Map PubKeyHash Integer
twoWallets k1 v1 k2 v2 = Map.unsafeFromList [(k1, v1), (k2, v2)]

walletsSum :: Map.Map PubKeyHash Integer -> Integer
walletsSum m = sum [v | (_, v) <- Map.toList m]

-- ============================================================================
-- Donate contexts
-- ============================================================================

mkDonateCtx :: CrowdfundDatum -> Integer -> PubKeyHash -> CrowdfundDatum -> ScriptContext
mkDonateCtx inputDatum amount donor outputDatum =
  let inputAda = walletsSum (cfWallets inputDatum)
      outputAda = inputAda + amount
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData (Donate amount donor))
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue (fromIntegral inputAda))
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner donor
            <> withOutput
              ( withTxOutAddress contractAddress
                  <> withTxOutValue (mkAdaValue (fromIntegral outputAda))
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange beforeDeadline
        )

mkDonateNewDonorCtx :: ScriptContext
mkDonateNewDonorCtx =
  let inputDatum = mkDatum emptyWallets
      outputDatum = mkDatum (singleWallet donor1Pkh 5_000_000)
   in mkDonateCtx inputDatum 5_000_000 donor1Pkh outputDatum

mkDonateExistingDonorCtx :: ScriptContext
mkDonateExistingDonorCtx =
  let inputDatum = mkDatum (singleWallet donor1Pkh 5_000_000)
      outputDatum = mkDatum (singleWallet donor1Pkh 8_000_000)
   in mkDonateCtx inputDatum 3_000_000 donor1Pkh outputDatum

mkDonateSecondDonorCtx :: ScriptContext
mkDonateSecondDonorCtx =
  let inputDatum = mkDatum (singleWallet donor1Pkh 5_000_000)
      outputDatum = mkDatum (twoWallets donor1Pkh 5_000_000 donor2Pkh 3_000_000)
   in mkDonateCtx inputDatum 3_000_000 donor2Pkh outputDatum

mkDonateAfterDeadlineCtx :: ScriptContext
mkDonateAfterDeadlineCtx =
  let inputDatum = mkDatum emptyWallets
      outputDatum = mkDatum (singleWallet donor1Pkh 5_000_000)
      inputAda = walletsSum (cfWallets inputDatum)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData (Donate 5_000_000 donor1Pkh))
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue (fromIntegral inputAda))
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner donor1Pkh
            <> withOutput
              ( withTxOutAddress contractAddress
                  <> withTxOutValue (mkAdaValue 5_000_000)
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange afterDeadline
        )

mkDonateWrongSignerCtx :: ScriptContext
mkDonateWrongSignerCtx =
  let inputDatum = mkDatum emptyWallets
      outputDatum = mkDatum (singleWallet donor1Pkh 5_000_000)
      inputAda = walletsSum (cfWallets inputDatum)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData (Donate 5_000_000 donor1Pkh))
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue (fromIntegral inputAda))
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner donor2Pkh
            <> withOutput
              ( withTxOutAddress contractAddress
                  <> withTxOutValue (mkAdaValue 5_000_000)
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange beforeDeadline
        )

mkDonateWrongOutputValueCtx :: ScriptContext
mkDonateWrongOutputValueCtx =
  let inputDatum = mkDatum emptyWallets
      outputDatum = mkDatum (singleWallet donor1Pkh 5_000_000)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData (Donate 5_000_000 donor1Pkh))
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue 0)
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner donor1Pkh
            <> withOutput
              ( withTxOutAddress contractAddress
                  <> withTxOutValue (mkAdaValue 99_000_000)
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange beforeDeadline
        )

mkDonateDatumGoalChangedCtx :: ScriptContext
mkDonateDatumGoalChangedCtx =
  let inputDatum = mkDatum emptyWallets
      outputDatum = (mkDatum (singleWallet donor1Pkh 5_000_000)){cfGoal = 999}
   in mkDonateCtx inputDatum 5_000_000 donor1Pkh outputDatum

mkDonateOtherWalletTamperedCtx :: ScriptContext
mkDonateOtherWalletTamperedCtx =
  let inputDatum = mkDatum (singleWallet donor1Pkh 5_000_000)
      outputDatum = mkDatum (twoWallets donor1Pkh 999 donor2Pkh 3_000_000)
   in mkDonateCtx inputDatum 3_000_000 donor2Pkh outputDatum

mkDonateDonorAmountWrongCtx :: ScriptContext
mkDonateDonorAmountWrongCtx =
  let inputDatum = mkDatum (singleWallet donor1Pkh 5_000_000)
      outputDatum = mkDatum (singleWallet donor1Pkh 99_000_000)
      inputAda = walletsSum (cfWallets inputDatum)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData (Donate 3_000_000 donor1Pkh))
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue (fromIntegral inputAda))
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner donor1Pkh
            <> withOutput
              ( withTxOutAddress contractAddress
                  <> withTxOutValue (mkAdaValue (fromIntegral (inputAda + 3_000_000)))
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange beforeDeadline
        )

mkDonateDonorMissingFromOutputCtx :: ScriptContext
mkDonateDonorMissingFromOutputCtx =
  let inputDatum = mkDatum emptyWallets
      outputDatum = mkDatum emptyWallets
   in mkDonateCtx inputDatum 5_000_000 donor1Pkh outputDatum

-- ============================================================================
-- Withdraw contexts
-- ============================================================================

mkWithdrawCtx :: CrowdfundDatum -> PubKeyHash -> POSIXTimeRange -> ScriptContext
mkWithdrawCtx inputDatum signer validRange =
  let inputAda = walletsSum (cfWallets inputDatum)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData Withdraw)
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue (fromIntegral inputAda))
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner signer
            <> withValidRange validRange
        )

mkWithdrawValidCtx :: ScriptContext
mkWithdrawValidCtx =
  let datum = mkDatum (twoWallets donor1Pkh 5_000_000 donor2Pkh 5_000_000)
   in mkWithdrawCtx datum recipientPkh afterDeadline

mkWithdrawBeforeDeadlineCtx :: ScriptContext
mkWithdrawBeforeDeadlineCtx =
  let datum = mkDatum (twoWallets donor1Pkh 5_000_000 donor2Pkh 5_000_000)
   in mkWithdrawCtx datum recipientPkh beforeDeadline

mkWithdrawGoalNotMetCtx :: ScriptContext
mkWithdrawGoalNotMetCtx =
  let datum = mkDatum (singleWallet donor1Pkh 3_000_000)
   in mkWithdrawCtx datum recipientPkh afterDeadline

mkWithdrawWrongSignerCtx :: ScriptContext
mkWithdrawWrongSignerCtx =
  let datum = mkDatum (twoWallets donor1Pkh 5_000_000 donor2Pkh 5_000_000)
   in mkWithdrawCtx datum donor1Pkh afterDeadline

-- ============================================================================
-- Reclaim contexts
-- ============================================================================

mkReclaimMultipleCtx :: ScriptContext
mkReclaimMultipleCtx =
  let inputDatum = mkDatum (twoWallets donor1Pkh 5_000_000 donor2Pkh 5_000_000)
      outputDatum = mkDatum (singleWallet donor2Pkh 5_000_000)
      inputAda = walletsSum (cfWallets inputDatum)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData Reclaim)
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue (fromIntegral inputAda))
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner donor1Pkh
            <> withOutput
              ( withTxOutAddress contractAddress
                  <> withTxOutValue (mkAdaValue 5_000_000)
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange afterDeadline
        )

mkReclaimLastDonorCtx :: ScriptContext
mkReclaimLastDonorCtx =
  let inputDatum = mkDatum (singleWallet donor1Pkh 5_000_000)
      inputAda = walletsSum (cfWallets inputDatum)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData Reclaim)
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue (fromIntegral inputAda))
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner donor1Pkh
            <> withValidRange afterDeadline
        )

mkReclaimBeforeDeadlineCtx :: ScriptContext
mkReclaimBeforeDeadlineCtx =
  let inputDatum = mkDatum (singleWallet donor1Pkh 5_000_000)
      inputAda = walletsSum (cfWallets inputDatum)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData Reclaim)
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue (fromIntegral inputAda))
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner donor1Pkh
            <> withValidRange beforeDeadline
        )

mkReclaimSignerNotInWalletsCtx :: ScriptContext
mkReclaimSignerNotInWalletsCtx =
  let inputDatum = mkDatum (singleWallet donor1Pkh 5_000_000)
      inputAda = walletsSum (cfWallets inputDatum)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData Reclaim)
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue (fromIntegral inputAda))
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner donor3Pkh
            <> withValidRange afterDeadline
        )

mkReclaimWrongOutputAmountCtx :: ScriptContext
mkReclaimWrongOutputAmountCtx =
  let inputDatum = mkDatum (twoWallets donor1Pkh 5_000_000 donor2Pkh 5_000_000)
      outputDatum = mkDatum (singleWallet donor2Pkh 5_000_000)
      inputAda = walletsSum (cfWallets inputDatum)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData Reclaim)
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue (fromIntegral inputAda))
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner donor1Pkh
            <> withOutput
              ( withTxOutAddress contractAddress
                  <> withTxOutValue (mkAdaValue 99_000_000)
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange afterDeadline
        )

mkReclaimWrongOutputDatumCtx :: ScriptContext
mkReclaimWrongOutputDatumCtx =
  let inputDatum = mkDatum (twoWallets donor1Pkh 5_000_000 donor2Pkh 5_000_000)
      outputDatum = mkDatum (twoWallets donor1Pkh 5_000_000 donor2Pkh 5_000_000)
      inputAda = walletsSum (cfWallets inputDatum)
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData Reclaim)
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue (fromIntegral inputAda))
                <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
            )
            <> withSigner donor1Pkh
            <> withOutput
              ( withTxOutAddress contractAddress
                  <> withTxOutValue (mkAdaValue 5_000_000)
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange afterDeadline
        )

-- ============================================================================
-- Test runner
-- ============================================================================

mkCrowdfundTests :: String -> Script -> TestTree
mkCrowdfundTests name script =
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
        [ testGroup
            "Donate"
            [ testCase "donate_new_donor_valid" $ assertSucceeds mkDonateNewDonorCtx
            , testCase "donate_existing_donor_valid" $ assertSucceeds mkDonateExistingDonorCtx
            , testCase "donate_second_donor_valid" $ assertSucceeds mkDonateSecondDonorCtx
            , testCase "donate_after_deadline_rejected" $ assertFails mkDonateAfterDeadlineCtx
            , testCase "donate_wrong_signer_rejected" $ assertFails mkDonateWrongSignerCtx
            , testCase "donate_wrong_output_value_rejected" $ assertFails mkDonateWrongOutputValueCtx
            , testCase "donate_datum_goal_changed_rejected" $ assertFails mkDonateDatumGoalChangedCtx
            , testCase "donate_other_wallet_tampered_rejected" $ assertFails mkDonateOtherWalletTamperedCtx
            , testCase "donate_donor_amount_wrong_rejected" $ assertFails mkDonateDonorAmountWrongCtx
            , testCase "donate_donor_missing_from_output_rejected" $ assertFails mkDonateDonorMissingFromOutputCtx
            ]
        , testGroup
            "Withdraw"
            [ testCase "withdraw_valid" $ assertSucceeds mkWithdrawValidCtx
            , testCase "withdraw_before_deadline_rejected" $ assertFails mkWithdrawBeforeDeadlineCtx
            , testCase "withdraw_goal_not_met_rejected" $ assertFails mkWithdrawGoalNotMetCtx
            , testCase "withdraw_wrong_signer_rejected" $ assertFails mkWithdrawWrongSignerCtx
            ]
        , testGroup
            "Reclaim"
            [ testCase "reclaim_multiple_donors_valid" $ assertSucceeds mkReclaimMultipleCtx
            , testCase "reclaim_last_donor_valid" $ assertSucceeds mkReclaimLastDonorCtx
            , testCase "reclaim_before_deadline_rejected" $ assertFails mkReclaimBeforeDeadlineCtx
            , testCase "reclaim_signer_not_in_wallets_rejected" $ assertFails mkReclaimSignerNotInWalletsCtx
            , testCase "reclaim_wrong_output_amount_rejected" $ assertFails mkReclaimWrongOutputAmountCtx
            , testCase "reclaim_wrong_output_datum_rejected" $ assertFails mkReclaimWrongOutputDatumCtx
            ]
        ]

-- ============================================================================
-- Conformance: both scripts must agree on all scenarios
-- ============================================================================

mkCrowdfundConformanceTests :: Script -> Script -> TestTree
mkCrowdfundConformanceTests plutarchScript plinthScript =
  let allContexts =
        [ ("donate_new_donor", mkDonateNewDonorCtx)
        , ("donate_existing_donor", mkDonateExistingDonorCtx)
        , ("donate_second_donor", mkDonateSecondDonorCtx)
        , ("donate_after_deadline", mkDonateAfterDeadlineCtx)
        , ("donate_wrong_signer", mkDonateWrongSignerCtx)
        , ("donate_wrong_output_value", mkDonateWrongOutputValueCtx)
        , ("donate_datum_goal_changed", mkDonateDatumGoalChangedCtx)
        , ("donate_other_wallet_tampered", mkDonateOtherWalletTamperedCtx)
        , ("donate_donor_amount_wrong", mkDonateDonorAmountWrongCtx)
        , ("donate_donor_missing_from_output", mkDonateDonorMissingFromOutputCtx)
        , ("withdraw_valid", mkWithdrawValidCtx)
        , ("withdraw_before_deadline", mkWithdrawBeforeDeadlineCtx)
        , ("withdraw_goal_not_met", mkWithdrawGoalNotMetCtx)
        , ("withdraw_wrong_signer", mkWithdrawWrongSignerCtx)
        , ("reclaim_multiple_donors", mkReclaimMultipleCtx)
        , ("reclaim_last_donor", mkReclaimLastDonorCtx)
        , ("reclaim_before_deadline", mkReclaimBeforeDeadlineCtx)
        , ("reclaim_signer_not_in_wallets", mkReclaimSignerNotInWalletsCtx)
        , ("reclaim_wrong_output_amount", mkReclaimWrongOutputAmountCtx)
        , ("reclaim_wrong_output_datum", mkReclaimWrongOutputDatumCtx)
        ]
      evalWith script ctx =
        let (res, _, _) = evalScript (applyArguments script [PlutusTx.toData ctx])
         in isRight res
      checkConformance (ctxName, ctx) =
        testCase ("conformance_" ++ ctxName) $
          assertBool
            ("Plutarch and Plinth disagree on " ++ ctxName)
            (evalWith plutarchScript ctx == evalWith plinthScript ctx)
   in testGroup "Plutarch-Plinth Conformance" (map checkConformance allContexts)

-- ============================================================================
-- Benchmark scenarios
-- ============================================================================

crowdfundBenchScenarios :: [(String, ScriptContext)]
crowdfundBenchScenarios =
  [ ("donate: new donor", mkDonateNewDonorCtx)
  , ("donate: existing donor", mkDonateExistingDonorCtx)
  , ("donate: second donor", mkDonateSecondDonorCtx)
  , ("donate: after deadline (reject)", mkDonateAfterDeadlineCtx)
  , ("donate: wrong signer (reject)", mkDonateWrongSignerCtx)
  , ("donate: wrong output value (reject)", mkDonateWrongOutputValueCtx)
  , ("donate: goal changed (reject)", mkDonateDatumGoalChangedCtx)
  , ("donate: wallet tampered (reject)", mkDonateOtherWalletTamperedCtx)
  , ("withdraw: valid", mkWithdrawValidCtx)
  , ("withdraw: before deadline (reject)", mkWithdrawBeforeDeadlineCtx)
  , ("withdraw: goal not met (reject)", mkWithdrawGoalNotMetCtx)
  , ("withdraw: wrong signer (reject)", mkWithdrawWrongSignerCtx)
  , ("reclaim: multiple donors", mkReclaimMultipleCtx)
  , ("reclaim: last donor", mkReclaimLastDonorCtx)
  , ("reclaim: before deadline (reject)", mkReclaimBeforeDeadlineCtx)
  , ("reclaim: not in wallets (reject)", mkReclaimSignerNotInWalletsCtx)
  , ("reclaim: wrong amount (reject)", mkReclaimWrongOutputAmountCtx)
  , ("reclaim: wrong datum (reject)", mkReclaimWrongOutputDatumCtx)
  ]
