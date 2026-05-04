{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:inline-constants #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-cse-iterations=8 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations-pir=24 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations-uplc=24 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Settings.Contracts.SettingsPlinth (
  plinthSettingsScript,
) where

import Plutarch.Script (Script (..))
import PlutusLedgerApi.Data.V3
import PlutusTx qualified
import PlutusTx.Code (getPlcNoAnn)
import PlutusTx.Data.AssocMap qualified as DMap
import PlutusTx.Data.List qualified as DList
import PlutusTx.Prelude
import UntypedPlutusCore qualified as UPLC

import Settings.Types.SettingsState (
  MultisigScriptD,
  SettingsDatumD,
  SettingsRedeemerD,
  sdAuthorizedScoopersD,
  sdAuthorizedStakingKeysD,
  sdBaseFeeD,
  sdExtensionsD,
  sdMetadataAdminD,
  sdPoolCreationFeeD,
  sdSettingsAdminD,
  sdSimpleFeeD,
  sdStrategyFeeD,
  sdTreasuryAddressD,
  sdTreasuryAdminD,
  sdTreasuryAllowanceD,
  pattern AfterD,
  pattern AllOfD,
  pattern AnyOfD,
  pattern AtLeastD,
  pattern BeforeD,
  pattern ScriptWitD,
  pattern SettingsAdminUpdateD,
  pattern SettingsDatumD,
  pattern SignatureD,
  pattern TreasuryAdminUpdateD,
 )

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

compiledCodeToScript :: PlutusTx.CompiledCode a -> Script
compiledCodeToScript code =
  let UPLC.Program ann ver body = getPlcNoAnn code
   in Script (UPLC.Program ann ver (UPLC.termMapNames UPLC.unNameDeBruijn body))

-- ============================================================================
-- 2. Multisig
-- ============================================================================

{-# INLINEABLE listElem #-}
listElem :: PubKeyHash -> DList.List PubKeyHash -> Bool
listElem = DList.elem

{-# INLINEABLE listAll #-}
listAll :: (a -> Bool) -> [a] -> Bool
listAll _ [] = True
listAll f (x : xs) = f x && listAll f xs

{-# INLINEABLE listAny #-}
listAny :: (a -> Bool) -> [a] -> Bool
listAny _ [] = False
listAny f (x : xs) = f x || listAny f xs

{-# INLINEABLE listCount #-}
listCount :: (a -> Bool) -> [a] -> Integer
listCount _ [] = 0
listCount f (x : xs) = if f x then 1 + listCount f xs else listCount f xs

{-# INLINEABLE pairsHasKey #-}
pairsHasKey :: Credential -> DMap.Map Credential Lovelace -> Bool
pairsHasKey = DMap.member

{-# INLINEABLE multisigSatisfied #-}
multisigSatisfied :: MultisigScriptD -> DList.List PubKeyHash -> POSIXTimeRange -> DMap.Map Credential Lovelace -> Bool
multisigSatisfied script signatories validRange wdrlMap =
  case script of
    SignatureD keyHash ->
      listElem keyHash signatories
    AllOfD scripts ->
      listAll (\s -> multisigSatisfied s signatories validRange wdrlMap) scripts
    AnyOfD scripts ->
      listAny (\s -> multisigSatisfied s signatories validRange wdrlMap) scripts
    AtLeastD required scripts ->
      required <= listCount (\s -> multisigSatisfied s signatories validRange wdrlMap) scripts
    BeforeD time ->
      case ivTo validRange of
        UpperBound (Finite (POSIXTime hi)) isInclusive ->
          if isInclusive then hi <= time else hi < time
        _ -> False
    AfterD time ->
      case ivFrom validRange of
        LowerBound (Finite (POSIXTime lo)) isInclusive ->
          if isInclusive then time <= lo else time < lo
        _ -> False
    ScriptWitD scriptHash ->
      pairsHasKey (ScriptCredential (ScriptHash (getPubKeyHash scriptHash))) wdrlMap

-- ============================================================================
-- 3. Utility functions
-- ============================================================================

{-# INLINEABLE findOwnInput #-}
findOwnInput :: ScriptContext -> TxInInfo
findOwnInput (ScriptContext txInfo _ (SpendingScript ref _)) =
  case DList.find (\inp -> txInInfoOutRef inp == ref) (txInfoInputs txInfo) of
    Just x -> x
    Nothing -> traceError "own input not found"
findOwnInput _ = traceError "not spending"

{-# INLINEABLE getOutputDatum #-}
getOutputDatum :: TxOut -> SettingsDatumD
getOutputDatum o =
  case txOutDatum o of
    OutputDatum (Datum d) -> unsafeFromBuiltinData d
    _ -> traceError "no inline datum"

{-# INLINEABLE valueWithoutLovelace #-}
valueWithoutLovelace :: Value -> Value
valueWithoutLovelace (Value v) =
  Value (DMap.delete (CurrencySymbol "") v)

{-# INLINEABLE listHeadOutput #-}
listHeadOutput :: DList.List TxOut -> TxOut
listHeadOutput = DList.head

{-# INLINEABLE valueIsZero #-}
valueIsZero :: MintValue -> Bool
valueIsZero mv = DMap.null (mintValueToMap mv)

-- ============================================================================
-- 4. Settings Admin Update
-- ============================================================================

{-# INLINEABLE checkSettingsAdminUpdate #-}
checkSettingsAdminUpdate :: SettingsDatumD -> SettingsDatumD -> TxInfo -> Bool
checkSettingsAdminUpdate inputDatum outputDatum tx =
  let admin = unsafeFromBuiltinData @MultisigScriptD (sdSettingsAdminD inputDatum)
      signedByAdmin =
        multisigSatisfied
          admin
          (txInfoSignatories tx)
          (txInfoValidRange tx)
          (txInfoWdrl tx)
      stakingKeysUnchanged = sdAuthorizedStakingKeysD inputDatum == sdAuthorizedStakingKeysD outputDatum
      treasuryAddrUnchanged = sdTreasuryAddressD inputDatum == sdTreasuryAddressD outputDatum
      treasuryAllowUnchanged = sdTreasuryAllowanceD inputDatum == sdTreasuryAllowanceD outputDatum
   in signedByAdmin
        && stakingKeysUnchanged
        && treasuryAddrUnchanged
        && treasuryAllowUnchanged

-- ============================================================================
-- 5. Treasury Admin Update
-- ============================================================================

{-# INLINEABLE checkTreasuryAdminUpdate #-}
checkTreasuryAdminUpdate :: SettingsDatumD -> SettingsDatumD -> TxInfo -> Bool
checkTreasuryAdminUpdate inputDatum outputDatum tx =
  let admin = unsafeFromBuiltinData @MultisigScriptD (sdTreasuryAdminD inputDatum)
      signedByAdmin =
        multisigSatisfied
          admin
          (txInfoSignatories tx)
          (txInfoValidRange tx)
          (txInfoWdrl tx)
      settingsAdminUnchanged = sdSettingsAdminD inputDatum == sdSettingsAdminD outputDatum
      metadataAdminUnchanged = sdMetadataAdminD inputDatum == sdMetadataAdminD outputDatum
      treasuryAdminUnchanged = sdTreasuryAdminD inputDatum == sdTreasuryAdminD outputDatum
      scoopersUnchanged = sdAuthorizedScoopersD inputDatum == sdAuthorizedScoopersD outputDatum
      baseFeeUnchanged = sdBaseFeeD inputDatum == sdBaseFeeD outputDatum
      simpleFeeUnchanged = sdSimpleFeeD inputDatum == sdSimpleFeeD outputDatum
      strategyFeeUnchanged = sdStrategyFeeD inputDatum == sdStrategyFeeD outputDatum
      poolCreationFeeUnchanged = sdPoolCreationFeeD inputDatum == sdPoolCreationFeeD outputDatum
      extensionsUnchanged = sdExtensionsD inputDatum == sdExtensionsD outputDatum
   in signedByAdmin
        && settingsAdminUnchanged
        && metadataAdminUnchanged
        && treasuryAdminUnchanged
        && scoopersUnchanged
        && baseFeeUnchanged
        && simpleFeeUnchanged
        && strategyFeeUnchanged
        && poolCreationFeeUnchanged
        && extensionsUnchanged

-- ============================================================================
-- 6. Main validator
-- ============================================================================

{-# INLINEABLE settingsValidator #-}
settingsValidator :: SettingsDatumD -> SettingsRedeemerD -> ScriptContext -> Bool
settingsValidator inputDatum redeemer ctx =
  let tx = scriptContextTxInfo ctx
      ownInput = findOwnInput ctx
      ownAddress = txOutAddress (txInInfoResolved ownInput)
      ownInputValue = txOutValue (txInInfoResolved ownInput)
      ownOutput = listHeadOutput (txInfoOutputs tx)
      ownOutputAddress = txOutAddress ownOutput
      ownOutputValue = txOutValue ownOutput
      outputDatum = getOutputDatum ownOutput
      valueNotChanged = valueWithoutLovelace ownOutputValue == valueWithoutLovelace ownInputValue
      noMint = valueIsZero (txInfoMint tx)
   in ownOutputAddress
        == ownAddress
        && valueNotChanged
        && noMint
        && case redeemer of
          SettingsAdminUpdateD -> checkSettingsAdminUpdate inputDatum outputDatum tx
          TreasuryAdminUpdateD -> checkTreasuryAdminUpdate inputDatum outputDatum tx

-- ============================================================================
-- 7. Entry point and compiled script
-- ============================================================================

{-# INLINEABLE mkSettingsValidator #-}
mkSettingsValidator :: BuiltinData -> ()
mkSettingsValidator ctxData =
  let ctx = unsafeFromBuiltinData @ScriptContext ctxData
   in case scriptContextScriptInfo ctx of
        SpendingScript _ (Just (Datum datumData)) ->
          let datum = unsafeFromBuiltinData @SettingsDatumD datumData
              redeemer = unsafeFromBuiltinData @SettingsRedeemerD (getRedeemer (scriptContextRedeemer ctx))
           in if settingsValidator datum redeemer ctx then () else error ()
        _ -> error ()

plinthSettingsScript :: Script
plinthSettingsScript =
  compiledCodeToScript $$(PlutusTx.compile [||mkSettingsValidator||])
