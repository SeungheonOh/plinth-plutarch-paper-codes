{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings.Test.Settings (
  mkSettingsTests,
  mkSettingsConformanceTests,
  settingsBenchScenarios,
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Settings.Types.SettingsState

-- ============================================================================
-- Constants
-- ============================================================================

bs28 :: Word8 -> BuiltinByteString
bs28 w = PV1.toBuiltin (BS.replicate 28 w)

settingsScriptHash :: ScriptHash
settingsScriptHash = ScriptHash (bs28 0x30)

settingsAddress :: Address
settingsAddress = Address (ScriptCredential settingsScriptHash) Nothing

settingsAdminPkh :: PubKeyHash
settingsAdminPkh = PubKeyHash (bs28 0x01)

treasuryAdminPkh :: PubKeyHash
treasuryAdminPkh = PubKeyHash (bs28 0x02)

metadataAdminPkh :: PubKeyHash
metadataAdminPkh = PubKeyHash (bs28 0x03)

randomPkh :: PubKeyHash
randomPkh = PubKeyHash (bs28 0x04)

scooper1Pkh :: PubKeyHash
scooper1Pkh = PubKeyHash (bs28 0x05)

scooper2Pkh :: PubKeyHash
scooper2Pkh = PubKeyHash (bs28 0x06)

settingsUtxoRef :: TxOutRef
settingsUtxoRef = TxOutRef (TxId (bs28 0xAA)) 0

pubKeyAddress :: PubKeyHash -> Address
pubKeyAddress pkh = Address (PubKeyCredential pkh) Nothing

settingsNftCS :: CurrencySymbol
settingsNftCS = CurrencySymbol (bs28 0x30)

settingsNftValue :: Value
settingsNftValue = mkAdaValue 2_000_000 <> mkTokenValue settingsNftCS (TokenName "settings") 1

mkTokenValue :: CurrencySymbol -> TokenName -> Integer -> Value
mkTokenValue cs tn amt =
  PV1.Value (Map.unsafeFromList [(cs, Map.unsafeFromList [(tn, amt)])])

stakingKey1 :: Credential
stakingKey1 = PubKeyCredential (PubKeyHash (bs28 0x10))

treasuryAddress :: Address
treasuryAddress = pubKeyAddress (PubKeyHash (bs28 0x20))

-- ============================================================================
-- Datum helpers
-- ============================================================================

mkDefaultDatum :: SettingsDatum
mkDefaultDatum =
  SettingsDatum
    { sdSettingsAdmin = Signature settingsAdminPkh
    , sdMetadataAdmin = pubKeyAddress metadataAdminPkh
    , sdTreasuryAdmin = Signature treasuryAdminPkh
    , sdTreasuryAddress = treasuryAddress
    , sdTreasuryAllowance = (1, 10)
    , sdAuthorizedScoopers = Just [scooper1Pkh]
    , sdAuthorizedStakingKeys = [stakingKey1]
    , sdBaseFee = 0
    , sdSimpleFee = 2_500_000
    , sdStrategyFee = 5_000_000
    , sdPoolCreationFee = 0
    , sdExtensions = PlutusTx.toBuiltinData (0 :: Integer)
    }

-- ============================================================================
-- Settings Admin Update contexts
-- ============================================================================

mkSettingsAdminCtx :: SettingsDatum -> SettingsDatum -> PubKeyHash -> ScriptContext
mkSettingsAdminCtx inputDatum outputDatum signer =
  buildScriptContext
    ( withSpendingScript
        (PlutusTx.toBuiltinData SettingsAdminUpdate)
        ( withOutRef settingsUtxoRef
            <> withAddress settingsAddress
            <> withValue settingsNftValue
            <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
        )
        <> withSigner signer
        <> withOutput
          ( withTxOutAddress settingsAddress
              <> withTxOutValue settingsNftValue
              <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
          )
    )

mkSettingsAdminUpdateScoopers :: ScriptContext
mkSettingsAdminUpdateScoopers =
  let outputDatum = mkDefaultDatum{sdAuthorizedScoopers = Just [scooper1Pkh, scooper2Pkh]}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminUpdateBaseFee :: ScriptContext
mkSettingsAdminUpdateBaseFee =
  let outputDatum = mkDefaultDatum{sdBaseFee = 500_000}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminUpdateMetadataAdmin :: ScriptContext
mkSettingsAdminUpdateMetadataAdmin =
  let outputDatum = mkDefaultDatum{sdMetadataAdmin = pubKeyAddress randomPkh}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminUpdateSettingsAdmin :: ScriptContext
mkSettingsAdminUpdateSettingsAdmin =
  let outputDatum = mkDefaultDatum{sdSettingsAdmin = Signature randomPkh}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminUpdateExtensions :: ScriptContext
mkSettingsAdminUpdateExtensions =
  let outputDatum = mkDefaultDatum{sdExtensions = PlutusTx.toBuiltinData (42 :: Integer)}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminChangeTreasuryAddressRejected :: ScriptContext
mkSettingsAdminChangeTreasuryAddressRejected =
  let outputDatum = mkDefaultDatum{sdTreasuryAddress = pubKeyAddress randomPkh}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminChangeStakingKeysRejected :: ScriptContext
mkSettingsAdminChangeStakingKeysRejected =
  let outputDatum = mkDefaultDatum{sdAuthorizedStakingKeys = [PubKeyCredential randomPkh]}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminChangeTreasuryAllowanceRejected :: ScriptContext
mkSettingsAdminChangeTreasuryAllowanceRejected =
  let outputDatum = mkDefaultDatum{sdTreasuryAllowance = (5, 10)}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminWrongSignerRejected :: ScriptContext
mkSettingsAdminWrongSignerRejected =
  let outputDatum = mkDefaultDatum{sdBaseFee = 500_000}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum randomPkh

mkSettingsAdminUpdateSimpleFee :: ScriptContext
mkSettingsAdminUpdateSimpleFee =
  let outputDatum = mkDefaultDatum{sdSimpleFee = 3_000_000}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminUpdateStrategyFee :: ScriptContext
mkSettingsAdminUpdateStrategyFee =
  let outputDatum = mkDefaultDatum{sdStrategyFee = 10_000_000}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminUpdatePoolCreationFee :: ScriptContext
mkSettingsAdminUpdatePoolCreationFee =
  let outputDatum = mkDefaultDatum{sdPoolCreationFee = 1_000_000}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminUpdateTreasuryAdmin :: ScriptContext
mkSettingsAdminUpdateTreasuryAdmin =
  let outputDatum = mkDefaultDatum{sdTreasuryAdmin = Signature randomPkh}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminUpdateMultipleFieldsValid :: ScriptContext
mkSettingsAdminUpdateMultipleFieldsValid =
  let outputDatum =
        mkDefaultDatum
          { sdBaseFee = 500_000
          , sdSimpleFee = 3_000_000
          , sdAuthorizedScoopers = Just [scooper1Pkh, scooper2Pkh]
          , sdMetadataAdmin = pubKeyAddress randomPkh
          }
   in mkSettingsAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminNoChangeValid :: ScriptContext
mkSettingsAdminNoChangeValid =
  mkSettingsAdminCtx mkDefaultDatum mkDefaultDatum settingsAdminPkh

-- ============================================================================
-- Treasury Admin Update contexts
-- ============================================================================

mkTreasuryAdminCtx :: SettingsDatum -> SettingsDatum -> PubKeyHash -> ScriptContext
mkTreasuryAdminCtx inputDatum outputDatum signer =
  buildScriptContext
    ( withSpendingScript
        (PlutusTx.toBuiltinData TreasuryAdminUpdate)
        ( withOutRef settingsUtxoRef
            <> withAddress settingsAddress
            <> withValue settingsNftValue
            <> withInlineDatum (PlutusTx.toBuiltinData inputDatum)
        )
        <> withSigner signer
        <> withOutput
          ( withTxOutAddress settingsAddress
              <> withTxOutValue settingsNftValue
              <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
          )
    )

mkTreasuryAdminUpdateTreasuryAddress :: ScriptContext
mkTreasuryAdminUpdateTreasuryAddress =
  let outputDatum = mkDefaultDatum{sdTreasuryAddress = pubKeyAddress randomPkh}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminUpdateStakingKeys :: ScriptContext
mkTreasuryAdminUpdateStakingKeys =
  let outputDatum = mkDefaultDatum{sdAuthorizedStakingKeys = [PubKeyCredential randomPkh]}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminUpdateAllowance :: ScriptContext
mkTreasuryAdminUpdateAllowance =
  let outputDatum = mkDefaultDatum{sdTreasuryAllowance = (5, 10)}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminChangeSettingsAdminRejected :: ScriptContext
mkTreasuryAdminChangeSettingsAdminRejected =
  let outputDatum = mkDefaultDatum{sdSettingsAdmin = Signature randomPkh}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminChangeMetadataAdminRejected :: ScriptContext
mkTreasuryAdminChangeMetadataAdminRejected =
  let outputDatum = mkDefaultDatum{sdMetadataAdmin = pubKeyAddress randomPkh}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminChangeScoopersRejected :: ScriptContext
mkTreasuryAdminChangeScoopersRejected =
  let outputDatum = mkDefaultDatum{sdAuthorizedScoopers = Just [scooper2Pkh]}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminChangeBaseFeeRejected :: ScriptContext
mkTreasuryAdminChangeBaseFeeRejected =
  let outputDatum = mkDefaultDatum{sdBaseFee = 999}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminChangeSimpleFeeRejected :: ScriptContext
mkTreasuryAdminChangeSimpleFeeRejected =
  let outputDatum = mkDefaultDatum{sdSimpleFee = 999}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminChangeStrategyFeeRejected :: ScriptContext
mkTreasuryAdminChangeStrategyFeeRejected =
  let outputDatum = mkDefaultDatum{sdStrategyFee = 999}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminChangePoolCreationFeeRejected :: ScriptContext
mkTreasuryAdminChangePoolCreationFeeRejected =
  let outputDatum = mkDefaultDatum{sdPoolCreationFee = 999}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminChangeExtensionsRejected :: ScriptContext
mkTreasuryAdminChangeExtensionsRejected =
  let outputDatum = mkDefaultDatum{sdExtensions = PlutusTx.toBuiltinData (99 :: Integer)}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminWrongSignerRejected :: ScriptContext
mkTreasuryAdminWrongSignerRejected =
  let outputDatum = mkDefaultDatum{sdTreasuryAddress = pubKeyAddress randomPkh}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum randomPkh

mkTreasuryAdminChangeTreasuryAdminRejected :: ScriptContext
mkTreasuryAdminChangeTreasuryAdminRejected =
  let outputDatum = mkDefaultDatum{sdTreasuryAdmin = Signature randomPkh}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminUpdateMultipleFieldsValid :: ScriptContext
mkTreasuryAdminUpdateMultipleFieldsValid =
  let outputDatum =
        mkDefaultDatum
          { sdTreasuryAddress = pubKeyAddress randomPkh
          , sdTreasuryAllowance = (5, 10)
          , sdAuthorizedStakingKeys = [PubKeyCredential randomPkh]
          }
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminNoChangeValid :: ScriptContext
mkTreasuryAdminNoChangeValid =
  mkTreasuryAdminCtx mkDefaultDatum mkDefaultDatum treasuryAdminPkh

-- ============================================================================
-- Value change / mint contexts
-- ============================================================================

mkSettingsAdminValueChangedRejected :: ScriptContext
mkSettingsAdminValueChangedRejected =
  let extraToken = mkTokenValue (CurrencySymbol (bs28 0xFF)) (TokenName "extra") 1
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkDefaultDatum)
            )
            <> withSigner settingsAdminPkh
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue (settingsNftValue <> extraToken)
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData mkDefaultDatum)
              )
        )

mkSettingsAdminMintRejected :: ScriptContext
mkSettingsAdminMintRejected =
  let mintedToken = mkTokenValue (CurrencySymbol (bs28 0xFF)) (TokenName "fake") 1
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkDefaultDatum)
            )
            <> withSigner settingsAdminPkh
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue settingsNftValue
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData mkDefaultDatum)
              )
            <> withMint mintedToken (PlutusTx.toBuiltinData ())
        )

mkSettingsAdminOutputAddressChangedRejected :: ScriptContext
mkSettingsAdminOutputAddressChangedRejected =
  buildScriptContext
    ( withSpendingScript
        (PlutusTx.toBuiltinData SettingsAdminUpdate)
        ( withOutRef settingsUtxoRef
            <> withAddress settingsAddress
            <> withValue settingsNftValue
            <> withInlineDatum (PlutusTx.toBuiltinData mkDefaultDatum)
        )
        <> withSigner settingsAdminPkh
        <> withOutput
          ( withTxOutAddress (pubKeyAddress randomPkh)
              <> withTxOutValue settingsNftValue
              <> withTxOutInlineDatum (PlutusTx.toBuiltinData mkDefaultDatum)
          )
    )

-- ============================================================================
-- Multisig contexts
-- ============================================================================

mkMultisigAllOfDatum :: SettingsDatum
mkMultisigAllOfDatum =
  mkDefaultDatum{sdSettingsAdmin = AllOf [Signature settingsAdminPkh, Signature randomPkh]}

mkMultisigAllOfValid :: ScriptContext
mkMultisigAllOfValid =
  let outputDatum = mkMultisigAllOfDatum{sdBaseFee = 999}
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkMultisigAllOfDatum)
            )
            <> withSigners [settingsAdminPkh, randomPkh]
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue settingsNftValue
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
        )

mkMultisigAllOfPartialRejected :: ScriptContext
mkMultisigAllOfPartialRejected =
  let outputDatum = mkMultisigAllOfDatum{sdBaseFee = 999}
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkMultisigAllOfDatum)
            )
            <> withSigner settingsAdminPkh
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue settingsNftValue
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
        )

mkMultisigAnyOfDatum :: SettingsDatum
mkMultisigAnyOfDatum =
  mkDefaultDatum{sdSettingsAdmin = AnyOf [Signature settingsAdminPkh, Signature randomPkh]}

mkMultisigAnyOfValid :: ScriptContext
mkMultisigAnyOfValid =
  let outputDatum = mkMultisigAnyOfDatum{sdBaseFee = 999}
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkMultisigAnyOfDatum)
            )
            <> withSigner randomPkh
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue settingsNftValue
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
        )

mkMultisigAtLeastDatum :: SettingsDatum
mkMultisigAtLeastDatum =
  mkDefaultDatum{sdSettingsAdmin = AtLeast 2 [Signature settingsAdminPkh, Signature treasuryAdminPkh, Signature randomPkh]}

mkMultisigAtLeastValid :: ScriptContext
mkMultisigAtLeastValid =
  let outputDatum = mkMultisigAtLeastDatum{sdBaseFee = 999}
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkMultisigAtLeastDatum)
            )
            <> withSigners [settingsAdminPkh, treasuryAdminPkh]
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue settingsNftValue
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
        )

mkMultisigAtLeastInsufficientRejected :: ScriptContext
mkMultisigAtLeastInsufficientRejected =
  let outputDatum = mkMultisigAtLeastDatum{sdBaseFee = 999}
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkMultisigAtLeastDatum)
            )
            <> withSigner settingsAdminPkh
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue settingsNftValue
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
        )

-- ============================================================================
-- Cross-redeemer / structural contexts
-- ============================================================================

mkSettingsAdminRedeemerTreasurySignerRejected :: ScriptContext
mkSettingsAdminRedeemerTreasurySignerRejected =
  let outputDatum = mkDefaultDatum{sdBaseFee = 500_000}
   in mkSettingsAdminCtx mkDefaultDatum outputDatum treasuryAdminPkh

mkTreasuryAdminRedeemerSettingsSignerRejected :: ScriptContext
mkTreasuryAdminRedeemerSettingsSignerRejected =
  let outputDatum = mkDefaultDatum{sdTreasuryAddress = pubKeyAddress randomPkh}
   in mkTreasuryAdminCtx mkDefaultDatum outputDatum settingsAdminPkh

mkSettingsAdminMissingDatumRejected :: ScriptContext
mkSettingsAdminMissingDatumRejected =
  buildScriptContext
    ( withSpendingScript
        (PlutusTx.toBuiltinData SettingsAdminUpdate)
        ( withOutRef settingsUtxoRef
            <> withAddress settingsAddress
            <> withValue settingsNftValue
            <> withInlineDatum (PlutusTx.toBuiltinData mkDefaultDatum)
        )
        <> withSigner settingsAdminPkh
        <> withOutput
          ( withTxOutAddress settingsAddress
              <> withTxOutValue settingsNftValue
          )
    )

-- ============================================================================
-- Multisig time-based contexts
-- ============================================================================

beforeDeadline :: POSIXTimeRange
beforeDeadline =
  Interval
    (LowerBound (Finite 500) True)
    (UpperBound (Finite 900) False)

afterDeadline :: POSIXTimeRange
afterDeadline =
  Interval
    (LowerBound (Finite 1500) True)
    (UpperBound (Finite 2000) False)

mkMultisigBeforeDatum :: SettingsDatum
mkMultisigBeforeDatum =
  mkDefaultDatum{sdSettingsAdmin = AllOf [Signature settingsAdminPkh, Before 1000]}

mkMultisigBeforeValid :: ScriptContext
mkMultisigBeforeValid =
  let outputDatum = mkMultisigBeforeDatum{sdBaseFee = 999}
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkMultisigBeforeDatum)
            )
            <> withSigner settingsAdminPkh
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue settingsNftValue
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange beforeDeadline
        )

mkMultisigBeforeExpiredRejected :: ScriptContext
mkMultisigBeforeExpiredRejected =
  let outputDatum = mkMultisigBeforeDatum{sdBaseFee = 999}
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkMultisigBeforeDatum)
            )
            <> withSigner settingsAdminPkh
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue settingsNftValue
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange afterDeadline
        )

mkMultisigAfterDatum :: SettingsDatum
mkMultisigAfterDatum =
  mkDefaultDatum{sdSettingsAdmin = AllOf [Signature settingsAdminPkh, After 1000]}

mkMultisigAfterValid :: ScriptContext
mkMultisigAfterValid =
  let outputDatum = mkMultisigAfterDatum{sdBaseFee = 999}
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkMultisigAfterDatum)
            )
            <> withSigner settingsAdminPkh
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue settingsNftValue
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange afterDeadline
        )

mkMultisigAfterTooEarlyRejected :: ScriptContext
mkMultisigAfterTooEarlyRejected =
  let outputDatum = mkMultisigAfterDatum{sdBaseFee = 999}
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkMultisigAfterDatum)
            )
            <> withSigner settingsAdminPkh
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue settingsNftValue
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData outputDatum)
              )
            <> withValidRange beforeDeadline
        )

-- ============================================================================
-- ADA-only change contexts (lovelace can change freely)
-- ============================================================================

mkSettingsAdminLovelaceChangeValid :: ScriptContext
mkSettingsAdminLovelaceChangeValid =
  let outputValue = mkAdaValue 5_000_000 <> mkTokenValue settingsNftCS (TokenName "settings") 1
   in buildScriptContext
        ( withSpendingScript
            (PlutusTx.toBuiltinData SettingsAdminUpdate)
            ( withOutRef settingsUtxoRef
                <> withAddress settingsAddress
                <> withValue settingsNftValue
                <> withInlineDatum (PlutusTx.toBuiltinData mkDefaultDatum)
            )
            <> withSigner settingsAdminPkh
            <> withOutput
              ( withTxOutAddress settingsAddress
                  <> withTxOutValue outputValue
                  <> withTxOutInlineDatum (PlutusTx.toBuiltinData mkDefaultDatum)
              )
        )

-- ============================================================================
-- Test runner
-- ============================================================================

mkSettingsTests :: String -> Script -> TestTree
mkSettingsTests name script =
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
            "Settings Admin Update"
            [ testCase "update_scoopers_valid" $ assertSucceeds mkSettingsAdminUpdateScoopers
            , testCase "update_base_fee_valid" $ assertSucceeds mkSettingsAdminUpdateBaseFee
            , testCase "update_simple_fee_valid" $ assertSucceeds mkSettingsAdminUpdateSimpleFee
            , testCase "update_strategy_fee_valid" $ assertSucceeds mkSettingsAdminUpdateStrategyFee
            , testCase "update_pool_creation_fee_valid" $ assertSucceeds mkSettingsAdminUpdatePoolCreationFee
            , testCase "update_metadata_admin_valid" $ assertSucceeds mkSettingsAdminUpdateMetadataAdmin
            , testCase "update_settings_admin_valid" $ assertSucceeds mkSettingsAdminUpdateSettingsAdmin
            , testCase "update_treasury_admin_valid" $ assertSucceeds mkSettingsAdminUpdateTreasuryAdmin
            , testCase "update_extensions_valid" $ assertSucceeds mkSettingsAdminUpdateExtensions
            , testCase "update_multiple_fields_valid" $ assertSucceeds mkSettingsAdminUpdateMultipleFieldsValid
            , testCase "no_change_valid" $ assertSucceeds mkSettingsAdminNoChangeValid
            , testCase "lovelace_change_valid" $ assertSucceeds mkSettingsAdminLovelaceChangeValid
            , testCase "change_treasury_address_rejected" $ assertFails mkSettingsAdminChangeTreasuryAddressRejected
            , testCase "change_staking_keys_rejected" $ assertFails mkSettingsAdminChangeStakingKeysRejected
            , testCase "change_treasury_allowance_rejected" $ assertFails mkSettingsAdminChangeTreasuryAllowanceRejected
            , testCase "wrong_signer_rejected" $ assertFails mkSettingsAdminWrongSignerRejected
            , testCase "cross_redeemer_wrong_admin_rejected" $ assertFails mkSettingsAdminRedeemerTreasurySignerRejected
            , testCase "value_changed_rejected" $ assertFails mkSettingsAdminValueChangedRejected
            , testCase "mint_rejected" $ assertFails mkSettingsAdminMintRejected
            , testCase "output_address_changed_rejected" $ assertFails mkSettingsAdminOutputAddressChangedRejected
            , testCase "missing_output_datum_rejected" $ assertFails mkSettingsAdminMissingDatumRejected
            ]
        , testGroup
            "Treasury Admin Update"
            [ testCase "update_treasury_address_valid" $ assertSucceeds mkTreasuryAdminUpdateTreasuryAddress
            , testCase "update_staking_keys_valid" $ assertSucceeds mkTreasuryAdminUpdateStakingKeys
            , testCase "update_allowance_valid" $ assertSucceeds mkTreasuryAdminUpdateAllowance
            , testCase "update_multiple_fields_valid" $ assertSucceeds mkTreasuryAdminUpdateMultipleFieldsValid
            , testCase "no_change_valid" $ assertSucceeds mkTreasuryAdminNoChangeValid
            , testCase "change_settings_admin_rejected" $ assertFails mkTreasuryAdminChangeSettingsAdminRejected
            , testCase "change_metadata_admin_rejected" $ assertFails mkTreasuryAdminChangeMetadataAdminRejected
            , testCase "change_treasury_admin_rejected" $ assertFails mkTreasuryAdminChangeTreasuryAdminRejected
            , testCase "change_scoopers_rejected" $ assertFails mkTreasuryAdminChangeScoopersRejected
            , testCase "change_base_fee_rejected" $ assertFails mkTreasuryAdminChangeBaseFeeRejected
            , testCase "change_simple_fee_rejected" $ assertFails mkTreasuryAdminChangeSimpleFeeRejected
            , testCase "change_strategy_fee_rejected" $ assertFails mkTreasuryAdminChangeStrategyFeeRejected
            , testCase "change_pool_creation_fee_rejected" $ assertFails mkTreasuryAdminChangePoolCreationFeeRejected
            , testCase "change_extensions_rejected" $ assertFails mkTreasuryAdminChangeExtensionsRejected
            , testCase "wrong_signer_rejected" $ assertFails mkTreasuryAdminWrongSignerRejected
            , testCase "cross_redeemer_wrong_admin_rejected" $ assertFails mkTreasuryAdminRedeemerSettingsSignerRejected
            ]
        , testGroup
            "Multisig"
            [ testCase "allof_both_signers_valid" $ assertSucceeds mkMultisigAllOfValid
            , testCase "allof_partial_rejected" $ assertFails mkMultisigAllOfPartialRejected
            , testCase "anyof_one_signer_valid" $ assertSucceeds mkMultisigAnyOfValid
            , testCase "atleast_sufficient_valid" $ assertSucceeds mkMultisigAtLeastValid
            , testCase "atleast_insufficient_rejected" $ assertFails mkMultisigAtLeastInsufficientRejected
            , testCase "before_valid" $ assertSucceeds mkMultisigBeforeValid
            , testCase "before_expired_rejected" $ assertFails mkMultisigBeforeExpiredRejected
            , testCase "after_valid" $ assertSucceeds mkMultisigAfterValid
            , testCase "after_too_early_rejected" $ assertFails mkMultisigAfterTooEarlyRejected
            ]
        ]

-- ============================================================================
-- Conformance: both scripts must agree on all scenarios
-- ============================================================================

mkSettingsConformanceTests :: Script -> Script -> TestTree
mkSettingsConformanceTests plutarchScript plinthScript =
  let allContexts =
        [ ("settings_admin_update_scoopers", mkSettingsAdminUpdateScoopers)
        , ("settings_admin_update_base_fee", mkSettingsAdminUpdateBaseFee)
        , ("settings_admin_update_simple_fee", mkSettingsAdminUpdateSimpleFee)
        , ("settings_admin_update_strategy_fee", mkSettingsAdminUpdateStrategyFee)
        , ("settings_admin_update_pool_creation_fee", mkSettingsAdminUpdatePoolCreationFee)
        , ("settings_admin_update_metadata_admin", mkSettingsAdminUpdateMetadataAdmin)
        , ("settings_admin_update_settings_admin", mkSettingsAdminUpdateSettingsAdmin)
        , ("settings_admin_update_treasury_admin", mkSettingsAdminUpdateTreasuryAdmin)
        , ("settings_admin_update_extensions", mkSettingsAdminUpdateExtensions)
        , ("settings_admin_update_multiple_fields", mkSettingsAdminUpdateMultipleFieldsValid)
        , ("settings_admin_no_change", mkSettingsAdminNoChangeValid)
        , ("settings_admin_lovelace_change", mkSettingsAdminLovelaceChangeValid)
        , ("settings_admin_change_treasury_address", mkSettingsAdminChangeTreasuryAddressRejected)
        , ("settings_admin_change_staking_keys", mkSettingsAdminChangeStakingKeysRejected)
        , ("settings_admin_change_treasury_allowance", mkSettingsAdminChangeTreasuryAllowanceRejected)
        , ("settings_admin_wrong_signer", mkSettingsAdminWrongSignerRejected)
        , ("settings_admin_cross_redeemer", mkSettingsAdminRedeemerTreasurySignerRejected)
        , ("settings_admin_value_changed", mkSettingsAdminValueChangedRejected)
        , ("settings_admin_mint", mkSettingsAdminMintRejected)
        , ("settings_admin_output_address_changed", mkSettingsAdminOutputAddressChangedRejected)
        , ("settings_admin_missing_datum", mkSettingsAdminMissingDatumRejected)
        , ("treasury_admin_update_treasury_address", mkTreasuryAdminUpdateTreasuryAddress)
        , ("treasury_admin_update_staking_keys", mkTreasuryAdminUpdateStakingKeys)
        , ("treasury_admin_update_allowance", mkTreasuryAdminUpdateAllowance)
        , ("treasury_admin_update_multiple_fields", mkTreasuryAdminUpdateMultipleFieldsValid)
        , ("treasury_admin_no_change", mkTreasuryAdminNoChangeValid)
        , ("treasury_admin_change_settings_admin", mkTreasuryAdminChangeSettingsAdminRejected)
        , ("treasury_admin_change_metadata_admin", mkTreasuryAdminChangeMetadataAdminRejected)
        , ("treasury_admin_change_treasury_admin", mkTreasuryAdminChangeTreasuryAdminRejected)
        , ("treasury_admin_change_scoopers", mkTreasuryAdminChangeScoopersRejected)
        , ("treasury_admin_change_base_fee", mkTreasuryAdminChangeBaseFeeRejected)
        , ("treasury_admin_change_simple_fee", mkTreasuryAdminChangeSimpleFeeRejected)
        , ("treasury_admin_change_strategy_fee", mkTreasuryAdminChangeStrategyFeeRejected)
        , ("treasury_admin_change_pool_creation_fee", mkTreasuryAdminChangePoolCreationFeeRejected)
        , ("treasury_admin_change_extensions", mkTreasuryAdminChangeExtensionsRejected)
        , ("treasury_admin_wrong_signer", mkTreasuryAdminWrongSignerRejected)
        , ("treasury_admin_cross_redeemer", mkTreasuryAdminRedeemerSettingsSignerRejected)
        , ("multisig_allof_valid", mkMultisigAllOfValid)
        , ("multisig_allof_partial", mkMultisigAllOfPartialRejected)
        , ("multisig_anyof_valid", mkMultisigAnyOfValid)
        , ("multisig_atleast_valid", mkMultisigAtLeastValid)
        , ("multisig_atleast_insufficient", mkMultisigAtLeastInsufficientRejected)
        , ("multisig_before_valid", mkMultisigBeforeValid)
        , ("multisig_before_expired", mkMultisigBeforeExpiredRejected)
        , ("multisig_after_valid", mkMultisigAfterValid)
        , ("multisig_after_too_early", mkMultisigAfterTooEarlyRejected)
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

settingsBenchScenarios :: [(String, ScriptContext)]
settingsBenchScenarios =
  [ ("settings admin: update scoopers", mkSettingsAdminUpdateScoopers)
  , ("settings admin: update base fee", mkSettingsAdminUpdateBaseFee)
  , ("settings admin: update simple fee", mkSettingsAdminUpdateSimpleFee)
  , ("settings admin: update strategy fee", mkSettingsAdminUpdateStrategyFee)
  , ("settings admin: update pool creation fee", mkSettingsAdminUpdatePoolCreationFee)
  , ("settings admin: update metadata admin", mkSettingsAdminUpdateMetadataAdmin)
  , ("settings admin: update settings admin", mkSettingsAdminUpdateSettingsAdmin)
  , ("settings admin: update treasury admin", mkSettingsAdminUpdateTreasuryAdmin)
  , ("settings admin: update extensions", mkSettingsAdminUpdateExtensions)
  , ("settings admin: update multiple fields", mkSettingsAdminUpdateMultipleFieldsValid)
  , ("settings admin: no change", mkSettingsAdminNoChangeValid)
  , ("settings admin: lovelace change", mkSettingsAdminLovelaceChangeValid)
  , ("settings admin: treasury addr (reject)", mkSettingsAdminChangeTreasuryAddressRejected)
  , ("settings admin: staking keys (reject)", mkSettingsAdminChangeStakingKeysRejected)
  , ("settings admin: wrong signer (reject)", mkSettingsAdminWrongSignerRejected)
  , ("settings admin: cross redeemer (reject)", mkSettingsAdminRedeemerTreasurySignerRejected)
  , ("settings admin: value changed (reject)", mkSettingsAdminValueChangedRejected)
  , ("settings admin: mint (reject)", mkSettingsAdminMintRejected)
  , ("settings admin: missing datum (reject)", mkSettingsAdminMissingDatumRejected)
  , ("treasury admin: update treasury addr", mkTreasuryAdminUpdateTreasuryAddress)
  , ("treasury admin: update staking keys", mkTreasuryAdminUpdateStakingKeys)
  , ("treasury admin: update allowance", mkTreasuryAdminUpdateAllowance)
  , ("treasury admin: update multiple fields", mkTreasuryAdminUpdateMultipleFieldsValid)
  , ("treasury admin: no change", mkTreasuryAdminNoChangeValid)
  , ("treasury admin: settings admin (reject)", mkTreasuryAdminChangeSettingsAdminRejected)
  , ("treasury admin: treasury admin (reject)", mkTreasuryAdminChangeTreasuryAdminRejected)
  , ("treasury admin: base fee (reject)", mkTreasuryAdminChangeBaseFeeRejected)
  , ("treasury admin: wrong signer (reject)", mkTreasuryAdminWrongSignerRejected)
  , ("treasury admin: cross redeemer (reject)", mkTreasuryAdminRedeemerSettingsSignerRejected)
  , ("multisig: allof valid", mkMultisigAllOfValid)
  , ("multisig: allof partial (reject)", mkMultisigAllOfPartialRejected)
  , ("multisig: anyof valid", mkMultisigAnyOfValid)
  , ("multisig: atleast valid", mkMultisigAtLeastValid)
  , ("multisig: atleast insufficient (reject)", mkMultisigAtLeastInsufficientRejected)
  , ("multisig: before valid", mkMultisigBeforeValid)
  , ("multisig: before expired (reject)", mkMultisigBeforeExpiredRejected)
  , ("multisig: after valid", mkMultisigAfterValid)
  , ("multisig: after too early (reject)", mkMultisigAfterTooEarlyRejected)
  ]
