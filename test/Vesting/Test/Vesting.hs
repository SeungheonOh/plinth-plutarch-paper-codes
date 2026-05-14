{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Vesting.Test.Vesting (
  mkVestingTests,
  mkVestingConformanceTests,
  vestingBenchScenarios,
) where

import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.Word (Word8)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Script (Script)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V3
import PlutusTx qualified
import ProgrammableTokens.Test.ScriptContext.Builder
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Vesting.Types.VestingState

-- ============================================================================
-- Constants
-- ============================================================================

bs28 :: Word8 -> BuiltinByteString
bs28 w = PV1.toBuiltin (BS.replicate 28 w)

contractScriptHash :: ScriptHash
contractScriptHash = ScriptHash (bs28 0x50)

contractAddress :: Address
contractAddress = Address (ScriptCredential contractScriptHash) Nothing

beneficiaryPkh :: PubKeyHash
beneficiaryPkh = PubKeyHash (bs28 0x01)

otherPkh :: PubKeyHash
otherPkh = PubKeyHash (bs28 0x02)

contractUtxoRef :: TxOutRef
contractUtxoRef = TxOutRef (TxId (bs28 0xAA)) 0

beneficiaryUtxoRef :: TxOutRef
beneficiaryUtxoRef = TxOutRef (TxId (bs28 0xBB)) 0

pubKeyAddr :: PubKeyHash -> Address
pubKeyAddr pkh = Address (PubKeyCredential pkh) Nothing

defaultFee :: Integer
defaultFee = 200_000

defaultBeneficiaryInputAda :: Integer
defaultBeneficiaryInputAda = 5_000_000

-- ============================================================================
-- Datum helpers
-- ============================================================================

defaultDatum :: VestingDatum
defaultDatum =
  VestingDatum
    { vdBeneficiary = beneficiaryPkh
    , vdStartTimestamp = 1_000
    , vdDuration = 10_000
    , vdAmount = 100_000_000
    }

oddDivisionDatum :: VestingDatum
oddDivisionDatum =
  VestingDatum
    { vdBeneficiary = beneficiaryPkh
    , vdStartTimestamp = 0
    , vdDuration = 7
    , vdAmount = 100
    }

-- ============================================================================
-- Validity ranges
-- ============================================================================

atTime :: Integer -> POSIXTimeRange
atTime t =
  Interval
    (LowerBound (Finite (POSIXTime t)) True)
    (UpperBound (Finite (POSIXTime (t + 100))) True)

negInfRange :: POSIXTimeRange
negInfRange =
  Interval
    (LowerBound NegInf True)
    (UpperBound (Finite (POSIXTime 99_999)) True)

posInfRange :: POSIXTimeRange
posInfRange =
  Interval
    (LowerBound PosInf True)
    (UpperBound PosInf True)

-- ============================================================================
-- Context builder
-- ============================================================================

mkReleaseCtx
  :: VestingDatum
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> POSIXTimeRange
  -> PubKeyHash
  -> Maybe VestingDatum
  -> ScriptContext
mkReleaseCtx inputDatum contractAmount declaredAmount beneInputAda fee validRange signer mOutputDatum =
  let beneficiaryOutputAda = declaredAmount + beneInputAda - fee
      redeemer = Release declaredAmount
      baseBuilder =
        withSpendingScript
          (PlutusTx.toBuiltinData redeemer)
          ( withOutRef contractUtxoRef
              <> withAddress contractAddress
              <> withValue (mkAdaValue (fromIntegral contractAmount))
              <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
          )
          <> withSigner signer
          <> withInput
            ( withOutRef beneficiaryUtxoRef
                <> withAddress (pubKeyAddr (vdBeneficiary inputDatum))
                <> withValue (mkAdaValue (fromIntegral beneInputAda))
            )
          <> withOutput
            ( withTxOutAddress (pubKeyAddr (vdBeneficiary inputDatum))
                <> withTxOutValue (mkAdaValue (fromIntegral beneficiaryOutputAda))
            )
          <> withValidRange validRange
          <> withFee fee
   in case mOutputDatum of
        Just outDatum ->
          let remainingAmount = contractAmount - declaredAmount
           in buildScriptContext
                ( baseBuilder
                    <> withOutput
                      ( withTxOutAddress contractAddress
                          <> withTxOutValue (mkAdaValue (fromIntegral remainingAmount))
                          <> withTxOutInlineDatum (PlutusTx.toBuiltinData outDatum)
                      )
                )
        Nothing -> buildScriptContext baseBuilder

mkReleaseCtxSplitBeneficiaryOutput
  :: VestingDatum
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> POSIXTimeRange
  -> [Integer]
  -> Maybe VestingDatum
  -> ScriptContext
mkReleaseCtxSplitBeneficiaryOutput inputDatum contractAmount declaredAmount beneInputAda fee validRange outputSplits mOutputDatum =
  let redeemer = Release declaredAmount
      baseBuilder =
        withSpendingScript
          (PlutusTx.toBuiltinData redeemer)
          ( withOutRef contractUtxoRef
              <> withAddress contractAddress
              <> withValue (mkAdaValue (fromIntegral contractAmount))
              <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
          )
          <> withSigner (vdBeneficiary inputDatum)
          <> withInput
            ( withOutRef beneficiaryUtxoRef
                <> withAddress (pubKeyAddr (vdBeneficiary inputDatum))
                <> withValue (mkAdaValue (fromIntegral beneInputAda))
            )
          <> mconcat
            [ withOutput
                ( withTxOutAddress (pubKeyAddr (vdBeneficiary inputDatum))
                    <> withTxOutValue (mkAdaValue (fromIntegral amt))
                )
            | amt <- outputSplits
            ]
          <> withValidRange validRange
          <> withFee fee
   in case mOutputDatum of
        Just outDatum ->
          let remainingAmount = contractAmount - declaredAmount
           in buildScriptContext
                ( baseBuilder
                    <> withOutput
                      ( withTxOutAddress contractAddress
                          <> withTxOutValue (mkAdaValue (fromIntegral remainingAmount))
                          <> withTxOutInlineDatum (PlutusTx.toBuiltinData outDatum)
                      )
                )
        Nothing -> buildScriptContext baseBuilder

-- ============================================================================
-- Positive test contexts
-- ============================================================================

mkFullWithdrawalAfterVestingCtx :: ScriptContext
mkFullWithdrawalAfterVestingCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    100_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 11_001)
    beneficiaryPkh
    Nothing

mkPartialWithdrawalMidpointCtx :: ScriptContext
mkPartialWithdrawalMidpointCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    50_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 6_000)
    beneficiaryPkh
    (Just defaultDatum)

mkSecondPartialWithdrawalCtx :: ScriptContext
mkSecondPartialWithdrawalCtx =
  mkReleaseCtx
    defaultDatum
    50_000_000
    20_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 8_000)
    beneficiaryPkh
    (Just defaultDatum)

mkFullWithdrawalAtEndCtx :: ScriptContext
mkFullWithdrawalAtEndCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    100_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 11_000)
    beneficiaryPkh
    Nothing

mkSmallWithdrawalEarlyCtx :: ScriptContext
mkSmallWithdrawalEarlyCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    10_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 1_001)
    beneficiaryPkh
    (Just defaultDatum)

mkThirdPartialDrainsRemainingCtx :: ScriptContext
mkThirdPartialDrainsRemainingCtx =
  mkReleaseCtx
    defaultDatum
    30_000_000
    30_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 11_001)
    beneficiaryPkh
    Nothing

mkOddDivisionPartialCtx :: ScriptContext
mkOddDivisionPartialCtx =
  let vested = 100 * 3 `div` 7
   in mkReleaseCtx
        oddDivisionDatum
        100
        vested
        defaultBeneficiaryInputAda
        defaultFee
        (atTime 3)
        beneficiaryPkh
        (Just oddDivisionDatum)

mkOddDivisionPartial2Ctx :: ScriptContext
mkOddDivisionPartial2Ctx =
  let remaining = 100 - (100 * 3 `div` 7)
      vested = 100 * 5 `div` 7
      released = 100 - remaining
      releaseAmount = vested - released
   in mkReleaseCtx
        oddDivisionDatum
        remaining
        releaseAmount
        defaultBeneficiaryInputAda
        defaultFee
        (atTime 5)
        beneficiaryPkh
        (Just oddDivisionDatum)

mkMultipleBeneficiaryOutputsCtx :: ScriptContext
mkMultipleBeneficiaryOutputsCtx =
  let totalBeneOutput = 100_000_000 + defaultBeneficiaryInputAda - defaultFee
      split1 = totalBeneOutput `div` 2
      split2 = totalBeneOutput - split1
   in mkReleaseCtxSplitBeneficiaryOutput
        defaultDatum
        100_000_000
        100_000_000
        defaultBeneficiaryInputAda
        defaultFee
        (atTime 11_001)
        [split1, split2]
        Nothing

mkZeroFeeFullWithdrawalCtx :: ScriptContext
mkZeroFeeFullWithdrawalCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    100_000_000
    defaultBeneficiaryInputAda
    0
    (atTime 11_001)
    beneficiaryPkh
    Nothing

mkQuarterVestedCtx :: ScriptContext
mkQuarterVestedCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    25_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 3_500)
    beneficiaryPkh
    (Just defaultDatum)

mkNinetyPercentVestedCtx :: ScriptContext
mkNinetyPercentVestedCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    90_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 10_000)
    beneficiaryPkh
    (Just defaultDatum)

-- ============================================================================
-- Negative test contexts
-- ============================================================================

mkNotSignedByBeneficiaryCtx :: ScriptContext
mkNotSignedByBeneficiaryCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    100_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 11_001)
    otherPkh
    Nothing

mkWrongDeclaredAmountCtx :: ScriptContext
mkWrongDeclaredAmountCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    80_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 6_000)
    beneficiaryPkh
    (Just defaultDatum)

mkOverClaimingCtx :: ScriptContext
mkOverClaimingCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    90_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 6_000)
    beneficiaryPkh
    (Just defaultDatum)

mkWrongBeneficiaryOutputCtx :: ScriptContext
mkWrongBeneficiaryOutputCtx =
  let redeemer = Release 100_000_000
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData redeemer)
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue 100_000_000)
                <> withInlineDatum (PlutusTx.toBuiltinData defaultDatum)
            )
            <> withSigner beneficiaryPkh
            <> withInput
              ( withOutRef beneficiaryUtxoRef
                  <> withAddress (pubKeyAddr beneficiaryPkh)
                  <> withValue (mkAdaValue (fromIntegral defaultBeneficiaryInputAda))
              )
            <> withOutput
              ( withTxOutAddress (pubKeyAddr beneficiaryPkh)
                  <> withTxOutValue (mkAdaValue 999_000_000)
              )
            <> withValidRange (atTime 11_001)
            <> withFee defaultFee
        )

mkPartialWithdrawalWrongDatumCtx :: ScriptContext
mkPartialWithdrawalWrongDatumCtx =
  let tamperedDatum =
        VestingDatum
          (vdBeneficiary defaultDatum)
          (vdStartTimestamp defaultDatum)
          (vdDuration defaultDatum)
          999_999_999
   in mkReleaseCtx
        defaultDatum
        100_000_000
        50_000_000
        defaultBeneficiaryInputAda
        defaultFee
        (atTime 6_000)
        beneficiaryPkh
        (Just tamperedDatum)

mkPartialWithdrawalNoContractOutputCtx :: ScriptContext
mkPartialWithdrawalNoContractOutputCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    50_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 6_000)
    beneficiaryPkh
    Nothing

mkPartialWithdrawalMultipleOutputsCtx :: ScriptContext
mkPartialWithdrawalMultipleOutputsCtx =
  let redeemer = Release 50_000_000
      beneficiaryOutputAda = 50_000_000 + defaultBeneficiaryInputAda - defaultFee
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData redeemer)
            ( withOutRef contractUtxoRef
                <> withAddress contractAddress
                <> withValue (mkAdaValue 100_000_000)
                <> withInlineDatum (PlutusTx.toBuiltinData defaultDatum)
            )
            <> withSigner beneficiaryPkh
            <> withInput
              ( withOutRef beneficiaryUtxoRef
                  <> withAddress (pubKeyAddr beneficiaryPkh)
                  <> withValue (mkAdaValue (fromIntegral defaultBeneficiaryInputAda))
              )
            <> withOutput
              ( withTxOutAddress (pubKeyAddr beneficiaryPkh)
                  <> withTxOutValue (mkAdaValue (fromIntegral beneficiaryOutputAda))
              )
            <> withOutput
              ( withTxOutAddress contractAddress
                  <> withTxOutValue (mkAdaValue 25_000_000)
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData defaultDatum)
              )
            <> withOutput
              ( withTxOutAddress contractAddress
                  <> withTxOutValue (mkAdaValue 25_000_000)
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData defaultDatum)
              )
            <> withValidRange (atTime 6_000)
            <> withFee defaultFee
        )

mkBeforeVestingStartsCtx :: ScriptContext
mkBeforeVestingStartsCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    50_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 500)
    beneficiaryPkh
    (Just defaultDatum)

mkPartialWithdrawalBeneficiaryChangedCtx :: ScriptContext
mkPartialWithdrawalBeneficiaryChangedCtx =
  let tamperedDatum =
        VestingDatum
          otherPkh
          (vdStartTimestamp defaultDatum)
          (vdDuration defaultDatum)
          (vdAmount defaultDatum)
   in mkReleaseCtx
        defaultDatum
        100_000_000
        50_000_000
        defaultBeneficiaryInputAda
        defaultFee
        (atTime 6_000)
        beneficiaryPkh
        (Just tamperedDatum)

mkPartialWithdrawalDurationChangedCtx :: ScriptContext
mkPartialWithdrawalDurationChangedCtx =
  let tamperedDatum =
        VestingDatum
          (vdBeneficiary defaultDatum)
          (vdStartTimestamp defaultDatum)
          1_000
          (vdAmount defaultDatum)
   in mkReleaseCtx
        defaultDatum
        100_000_000
        50_000_000
        defaultBeneficiaryInputAda
        defaultFee
        (atTime 6_000)
        beneficiaryPkh
        (Just tamperedDatum)

mkPartialWithdrawalStartChangedCtx :: ScriptContext
mkPartialWithdrawalStartChangedCtx =
  let tamperedDatum = VestingDatum (vdBeneficiary defaultDatum) 0 (vdDuration defaultDatum) (vdAmount defaultDatum)
   in mkReleaseCtx
        defaultDatum
        100_000_000
        50_000_000
        defaultBeneficiaryInputAda
        defaultFee
        (atTime 6_000)
        beneficiaryPkh
        (Just tamperedDatum)

mkNegInfRangeClaimCtx :: ScriptContext
mkNegInfRangeClaimCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    50_000_000
    defaultBeneficiaryInputAda
    defaultFee
    negInfRange
    beneficiaryPkh
    (Just defaultDatum)

mkPosInfRangeClaimCtx :: ScriptContext
mkPosInfRangeClaimCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    50_000_000
    defaultBeneficiaryInputAda
    defaultFee
    posInfRange
    beneficiaryPkh
    (Just defaultDatum)

mkExactStartTimeDeclareNonZeroCtx :: ScriptContext
mkExactStartTimeDeclareNonZeroCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    1
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 1_000)
    beneficiaryPkh
    (Just defaultDatum)

mkUnderClaimingCtx :: ScriptContext
mkUnderClaimingCtx =
  mkReleaseCtx
    defaultDatum
    100_000_000
    30_000_000
    defaultBeneficiaryInputAda
    defaultFee
    (atTime 6_000)
    beneficiaryPkh
    (Just defaultDatum)

mkOddDivisionWrongAmountCtx :: ScriptContext
mkOddDivisionWrongAmountCtx =
  let wrongVested = 100 * 3 `div` 7 + 1
   in mkReleaseCtx
        oddDivisionDatum
        100
        wrongVested
        defaultBeneficiaryInputAda
        defaultFee
        (atTime 3)
        beneficiaryPkh
        (Just oddDivisionDatum)

-- ============================================================================
-- Test runner
-- ============================================================================

mkVestingTests :: String -> Script -> TestTree
mkVestingTests name script =
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
            "Positive"
            [ testCase "full_withdrawal_after_vesting" $ assertSucceeds mkFullWithdrawalAfterVestingCtx
            , testCase "partial_withdrawal_midpoint" $ assertSucceeds mkPartialWithdrawalMidpointCtx
            , testCase "second_partial_withdrawal" $ assertSucceeds mkSecondPartialWithdrawalCtx
            , testCase "full_withdrawal_at_end" $ assertSucceeds mkFullWithdrawalAtEndCtx
            , testCase "small_withdrawal_early" $ assertSucceeds mkSmallWithdrawalEarlyCtx
            , testCase "third_partial_drains_remaining" $ assertSucceeds mkThirdPartialDrainsRemainingCtx
            , testCase "odd_division_partial" $ assertSucceeds mkOddDivisionPartialCtx
            , testCase "odd_division_partial_second" $ assertSucceeds mkOddDivisionPartial2Ctx
            , testCase "multiple_beneficiary_outputs" $ assertSucceeds mkMultipleBeneficiaryOutputsCtx
            , testCase "zero_fee_full_withdrawal" $ assertSucceeds mkZeroFeeFullWithdrawalCtx
            , testCase "quarter_vested" $ assertSucceeds mkQuarterVestedCtx
            , testCase "ninety_percent_vested" $ assertSucceeds mkNinetyPercentVestedCtx
            ]
        , testGroup
            "Negative"
            [ testCase "not_signed_by_beneficiary" $ assertFails mkNotSignedByBeneficiaryCtx
            , testCase "wrong_declared_amount" $ assertFails mkWrongDeclaredAmountCtx
            , testCase "over_claiming" $ assertFails mkOverClaimingCtx
            , testCase "under_claiming" $ assertFails mkUnderClaimingCtx
            , testCase "wrong_beneficiary_output" $ assertFails mkWrongBeneficiaryOutputCtx
            , testCase "partial_wrong_datum" $ assertFails mkPartialWithdrawalWrongDatumCtx
            , testCase "partial_no_contract_output" $ assertFails mkPartialWithdrawalNoContractOutputCtx
            , testCase "partial_multiple_contract_outputs" $ assertFails mkPartialWithdrawalMultipleOutputsCtx
            , testCase "before_vesting_starts" $ assertFails mkBeforeVestingStartsCtx
            , testCase "partial_beneficiary_changed" $ assertFails mkPartialWithdrawalBeneficiaryChangedCtx
            , testCase "partial_duration_changed" $ assertFails mkPartialWithdrawalDurationChangedCtx
            , testCase "partial_start_changed" $ assertFails mkPartialWithdrawalStartChangedCtx
            , testCase "neginf_range_nonzero_claim" $ assertFails mkNegInfRangeClaimCtx
            , testCase "posinf_range_nonzero_claim" $ assertFails mkPosInfRangeClaimCtx
            , testCase "exact_start_time_nonzero_claim" $ assertFails mkExactStartTimeDeclareNonZeroCtx
            , testCase "odd_division_off_by_one" $ assertFails mkOddDivisionWrongAmountCtx
            ]
        ]

-- ============================================================================
-- Conformance: both scripts must agree on all scenarios
-- ============================================================================

mkVestingConformanceTests :: Script -> Script -> TestTree
mkVestingConformanceTests plutarchScript plinthScript =
  let allContexts =
        [ ("full_withdrawal_after_vesting", mkFullWithdrawalAfterVestingCtx)
        , ("partial_withdrawal_midpoint", mkPartialWithdrawalMidpointCtx)
        , ("second_partial_withdrawal", mkSecondPartialWithdrawalCtx)
        , ("full_withdrawal_at_end", mkFullWithdrawalAtEndCtx)
        , ("small_withdrawal_early", mkSmallWithdrawalEarlyCtx)
        , ("third_partial_drains_remaining", mkThirdPartialDrainsRemainingCtx)
        , ("odd_division_partial", mkOddDivisionPartialCtx)
        , ("odd_division_partial_second", mkOddDivisionPartial2Ctx)
        , ("multiple_beneficiary_outputs", mkMultipleBeneficiaryOutputsCtx)
        , ("zero_fee_full_withdrawal", mkZeroFeeFullWithdrawalCtx)
        , ("quarter_vested", mkQuarterVestedCtx)
        , ("ninety_percent_vested", mkNinetyPercentVestedCtx)
        , ("not_signed_by_beneficiary", mkNotSignedByBeneficiaryCtx)
        , ("wrong_declared_amount", mkWrongDeclaredAmountCtx)
        , ("over_claiming", mkOverClaimingCtx)
        , ("under_claiming", mkUnderClaimingCtx)
        , ("wrong_beneficiary_output", mkWrongBeneficiaryOutputCtx)
        , ("partial_wrong_datum", mkPartialWithdrawalWrongDatumCtx)
        , ("partial_no_contract_output", mkPartialWithdrawalNoContractOutputCtx)
        , ("partial_multiple_contract_outputs", mkPartialWithdrawalMultipleOutputsCtx)
        , ("before_vesting_starts", mkBeforeVestingStartsCtx)
        , ("partial_beneficiary_changed", mkPartialWithdrawalBeneficiaryChangedCtx)
        , ("partial_duration_changed", mkPartialWithdrawalDurationChangedCtx)
        , ("partial_start_changed", mkPartialWithdrawalStartChangedCtx)
        , ("neginf_range_nonzero_claim", mkNegInfRangeClaimCtx)
        , ("posinf_range_nonzero_claim", mkPosInfRangeClaimCtx)
        , ("exact_start_time_nonzero_claim", mkExactStartTimeDeclareNonZeroCtx)
        , ("odd_division_off_by_one", mkOddDivisionWrongAmountCtx)
        ]
      evalWith script ctx =
        let (res, _, _) = evalScript (applyArguments script [PlutusTx.toData ctx])
         in isRight res
      checkConformance (ctxName, ctx) =
        testCase ("conformance_" ++ ctxName) $
          assertBool
            ("Plutarch and Plinth disagree on " ++ ctxName)
            (evalWith plutarchScript ctx == evalWith plinthScript ctx)
   in testGroup
        "Plutarch-Plinth Conformance"
        ( map checkConformance allContexts
            ++ [ propConformanceFullAmount plutarchScript plinthScript
               , propConformancePartialState plutarchScript plinthScript
               , propConformanceDivisionPrecision plutarchScript plinthScript
               ]
        )

-- ============================================================================
-- QuickCheck property: both agree on fresh vesting (full contract amount)
-- ============================================================================

propConformanceFullAmount :: Script -> Script -> TestTree
propConformanceFullAmount plutarchScript plinthScript =
  testProperty "prop_agree_full_amount (500 tests)" $
    QC.withMaxSuccess 500 $
      QC.forAll genVestingDatum $ \datum ->
        QC.forAll (genFullAmountContext datum) $ \ctx ->
          let evalWith script = isRight $ (\(r, _, _) -> r) $ evalScript (applyArguments script [PlutusTx.toData ctx])
           in evalWith plutarchScript QC.=== evalWith plinthScript

-- ============================================================================
-- QuickCheck property: both agree on partially-withdrawn states
-- ============================================================================

propConformancePartialState :: Script -> Script -> TestTree
propConformancePartialState plutarchScript plinthScript =
  testProperty "prop_agree_partial_state (500 tests)" $
    QC.withMaxSuccess 500 $
      QC.forAll genVestingDatum $ \datum ->
        QC.forAll (genPartialStateContext datum) $ \ctx ->
          let evalWith script = isRight $ (\(r, _, _) -> r) $ evalScript (applyArguments script [PlutusTx.toData ctx])
           in evalWith plutarchScript QC.=== evalWith plinthScript

-- ============================================================================
-- QuickCheck property: integer division precision alignment
-- ============================================================================

propConformanceDivisionPrecision :: Script -> Script -> TestTree
propConformanceDivisionPrecision plutarchScript plinthScript =
  testProperty "prop_agree_division_precision (500 tests)" $
    QC.withMaxSuccess 500 $
      QC.forAll genOddDivisionContext $ \ctx ->
        let evalWith script = isRight $ (\(r, _, _) -> r) $ evalScript (applyArguments script [PlutusTx.toData ctx])
         in evalWith plutarchScript QC.=== evalWith plinthScript

-- ============================================================================
-- QuickCheck generators
-- ============================================================================

genVestingDatum :: QC.Gen VestingDatum
genVestingDatum = do
  startTs <- QC.chooseInteger (0, 10_000)
  dur <- QC.chooseInteger (1_000, 50_000)
  amt <- QC.chooseInteger (1_000_000, 1_000_000_000)
  beneficiary <- QC.elements [beneficiaryPkh, otherPkh]
  pure $
    VestingDatum
      { vdBeneficiary = beneficiary
      , vdStartTimestamp = startTs
      , vdDuration = dur
      , vdAmount = amt
      }

linearVestingCalc :: Integer -> Integer -> Integer -> Integer -> Integer
linearVestingCalc startTs dur totalAmt ts
  | ts < startTs = 0
  | ts > startTs + dur = totalAmt
  | otherwise = (totalAmt * (ts - startTs)) `div` dur

genFullAmountContext :: VestingDatum -> QC.Gen ScriptContext
genFullAmountContext datum = do
  timestamp <- QC.chooseInteger (0, vdStartTimestamp datum + vdDuration datum + 10_000)
  let contractAmount = vdAmount datum
      linearVested = linearVestingCalc (vdStartTimestamp datum) (vdDuration datum) (vdAmount datum) timestamp
      released = vdAmount datum - contractAmount
      correctRelease = linearVested - released
  declaredAmount <-
    QC.frequency [(6, pure correctRelease), (4, QC.chooseInteger (-1, vdAmount datum + 1))]
  signer <- QC.frequency [(7, pure (vdBeneficiary datum)), (3, pure otherPkh)]
  fee <- QC.elements [0, defaultFee, 500_000]
  let beneInputAda = defaultBeneficiaryInputAda
      validRange = atTime timestamp
      isFullWithdrawal = declaredAmount == contractAmount
  useCorrectOutput <- QC.frequency [(7, pure True), (3, pure False)]
  let mOutputDatum
        | isFullWithdrawal = Nothing
        | useCorrectOutput = Just datum
        | otherwise =
            Just
              (VestingDatum (vdBeneficiary datum) (vdStartTimestamp datum) (vdDuration datum) (vdAmount datum + 1))
  pure $
    mkReleaseCtx
      datum
      contractAmount
      declaredAmount
      beneInputAda
      fee
      validRange
      signer
      mOutputDatum

genPartialStateContext :: VestingDatum -> QC.Gen ScriptContext
genPartialStateContext datum = do
  previousWithdrawFraction <- QC.chooseInteger (1, 9)
  let contractAmount = vdAmount datum - (vdAmount datum * previousWithdrawFraction `div` 10)
  timestamp <-
    QC.chooseInteger (vdStartTimestamp datum, vdStartTimestamp datum + vdDuration datum + 5_000)
  let linearVested = linearVestingCalc (vdStartTimestamp datum) (vdDuration datum) (vdAmount datum) timestamp
      released = vdAmount datum - contractAmount
      correctRelease = linearVested - released
  declaredAmount <-
    QC.frequency [(6, pure correctRelease), (4, QC.chooseInteger (-1, contractAmount + 1))]
  signer <- QC.frequency [(8, pure (vdBeneficiary datum)), (2, pure otherPkh)]
  let fee = defaultFee
      beneInputAda = defaultBeneficiaryInputAda
      validRange = atTime timestamp
      isFullWithdrawal = declaredAmount == contractAmount
  useCorrectOutput <- QC.frequency [(7, pure True), (3, pure False)]
  let mOutputDatum
        | isFullWithdrawal = Nothing
        | useCorrectOutput = Just datum
        | otherwise =
            Just
              (VestingDatum (vdBeneficiary datum) (vdStartTimestamp datum) (vdDuration datum + 1) (vdAmount datum))
  pure $
    mkReleaseCtx
      datum
      contractAmount
      declaredAmount
      beneInputAda
      fee
      validRange
      signer
      mOutputDatum

genOddDivisionContext :: QC.Gen ScriptContext
genOddDivisionContext = do
  dur <- QC.chooseInteger (3, 13)
  amt <- QC.chooseInteger (7, 199)
  let d =
        VestingDatum
          { vdBeneficiary = beneficiaryPkh
          , vdStartTimestamp = 0
          , vdDuration = dur
          , vdAmount = amt
          }
  timestamp <- QC.chooseInteger (1, dur - 1)
  let correctRelease = linearVestingCalc 0 dur amt timestamp
  declaredAmount <-
    QC.frequency
      [(5, pure correctRelease), (3, pure (correctRelease + 1)), (2, pure (correctRelease - 1))]
  let fee = defaultFee
      beneInputAda = defaultBeneficiaryInputAda
      validRange = atTime timestamp
      isFullWithdrawal = declaredAmount == amt
      mOutputDatum =
        if isFullWithdrawal
          then Nothing
          else Just d
  pure $
    mkReleaseCtx
      d
      amt
      declaredAmount
      beneInputAda
      fee
      validRange
      beneficiaryPkh
      mOutputDatum

-- ============================================================================
-- Benchmark scenarios
-- ============================================================================

vestingBenchScenarios :: [(String, ScriptContext)]
vestingBenchScenarios =
  [ ("full withdrawal after vesting", mkFullWithdrawalAfterVestingCtx)
  , ("partial withdrawal midpoint", mkPartialWithdrawalMidpointCtx)
  , ("second partial withdrawal", mkSecondPartialWithdrawalCtx)
  , ("full withdrawal at end", mkFullWithdrawalAtEndCtx)
  , ("small withdrawal early", mkSmallWithdrawalEarlyCtx)
  , ("third partial drains", mkThirdPartialDrainsRemainingCtx)
  , ("odd division partial", mkOddDivisionPartialCtx)
  , ("multi beneficiary outputs", mkMultipleBeneficiaryOutputsCtx)
  , ("zero fee", mkZeroFeeFullWithdrawalCtx)
  , ("quarter vested", mkQuarterVestedCtx)
  , ("ninety percent", mkNinetyPercentVestedCtx)
  , ("not signed (reject)", mkNotSignedByBeneficiaryCtx)
  , ("wrong amount (reject)", mkWrongDeclaredAmountCtx)
  , ("over claiming (reject)", mkOverClaimingCtx)
  , ("under claiming (reject)", mkUnderClaimingCtx)
  , ("wrong bene output (reject)", mkWrongBeneficiaryOutputCtx)
  , ("partial wrong datum (reject)", mkPartialWithdrawalWrongDatumCtx)
  , ("partial no output (reject)", mkPartialWithdrawalNoContractOutputCtx)
  , ("before vesting (reject)", mkBeforeVestingStartsCtx)
  , ("neginf range (reject)", mkNegInfRangeClaimCtx)
  , ("odd div off-by-one (reject)", mkOddDivisionWrongAmountCtx)
  ]
