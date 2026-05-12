{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:inline-callsite-growth=20 #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:inline-unconditional-growth=20 #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:no-preserve-logging #-}

module Settings.Contracts.SettingsPlinth (
  plinthSettingsScript,
) where

import Plinth.Plugin
import Plutarch.Script (Script (..))
import PlutusLedgerApi.Data.V3
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
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

{-# INLINEABLE pairsHasKey #-}
pairsHasKey :: Credential -> DMap.Map Credential Lovelace -> Bool
pairsHasKey = DMap.member

{-# INLINEABLE multisigSatisfied #-}
multisigSatisfied :: MultisigScriptD -> DList.List PubKeyHash -> POSIXTimeRange -> DMap.Map Credential Lovelace -> Bool
multisigSatisfied script signatories validRange wdrlMap = go script
 where
  go = \case
    SignatureD keyHash ->
      DList.elem keyHash signatories
    AllOfD scripts ->
      DList.all go scripts
    AnyOfD scripts ->
      DList.any go scripts
    AtLeastD required scripts ->
      required <= DList.foldr (\sig acc -> acc + (if go sig then 1 else 0)) 0 scripts
    BeforeD time -> case validRange of
      Interval _ (UpperBound ext isInclusive) -> case ext of
        Finite (POSIXTime hi) -> if isInclusive then hi <= time else hi < time
        _ -> False
    AfterD time -> case validRange of
      Interval (LowerBound ext isInclusive) _ -> case ext of
        Finite (POSIXTime lo) -> if isInclusive then time <= lo else time < lo
        _ -> False
    ScriptWitD scriptHash ->
      pairsHasKey (ScriptCredential (ScriptHash (getPubKeyHash scriptHash))) wdrlMap

-- ============================================================================
-- 3. Utility functions
-- ============================================================================

{-# INLINEABLE findOwnInput #-}
findOwnInput :: TxOutRef -> DList.List TxInInfo -> TxInInfo
findOwnInput ref inputs =
  case DList.find (\inp -> txInInfoOutRef inp == ref) inputs of
    Just x -> x
    Nothing -> error ()

{-# INLINEABLE getOutputDatum #-}
getOutputDatum :: TxOut -> SettingsDatumD
getOutputDatum o = case txOutDatum o of
  OutputDatum (Datum d) -> unsafeFromBuiltinData d
  _ -> error ()

{-# INLINEABLE valueWithoutLovelace #-}
valueWithoutLovelace :: Value -> Value
valueWithoutLovelace (Value v) =
  Value (DMap.delete (CurrencySymbol "") v)

{-# INLINEABLE valueIsZero #-}
valueIsZero :: MintValue -> Bool
valueIsZero mv = DMap.null (mintValueToMap mv)

-- ============================================================================
-- 4. Settings Admin Update
-- ============================================================================

{-# INLINEABLE checkSettingsAdminUpdate #-}
checkSettingsAdminUpdate
  :: SettingsDatumD
  -> SettingsDatumD
  -> DList.List PubKeyHash
  -> POSIXTimeRange
  -> DMap.Map Credential Lovelace
  -> Bool
checkSettingsAdminUpdate inputDatum outputDatum sigs validRange wdrl =
  let SettingsDatumD
        { sdSettingsAdminD = inSettingsAdmin
        , sdTreasuryAddressD = inTreasuryAddress
        , sdTreasuryAllowanceD = inTreasuryAllowance
        , sdAuthorizedStakingKeysD = inAuthorizedStakingKeys
        } = inputDatum
      SettingsDatumD
        { sdTreasuryAddressD = outTreasuryAddress
        , sdTreasuryAllowanceD = outTreasuryAllowance
        , sdAuthorizedStakingKeysD = outAuthorizedStakingKeys
        } = outputDatum
      admin = unsafeFromBuiltinData @MultisigScriptD inSettingsAdmin
      signedByAdmin = multisigSatisfied admin sigs validRange wdrl
      stakingKeysUnchanged = inAuthorizedStakingKeys == outAuthorizedStakingKeys
      treasuryAddrUnchanged = inTreasuryAddress == outTreasuryAddress
      treasuryAllowUnchanged = inTreasuryAllowance == outTreasuryAllowance
   in signedByAdmin
        && stakingKeysUnchanged
        && treasuryAddrUnchanged
        && treasuryAllowUnchanged

-- ============================================================================
-- 5. Treasury Admin Update
-- ============================================================================

{-# INLINEABLE checkTreasuryAdminUpdate #-}
checkTreasuryAdminUpdate
  :: SettingsDatumD
  -> SettingsDatumD
  -> DList.List PubKeyHash
  -> POSIXTimeRange
  -> DMap.Map Credential Lovelace
  -> Bool
checkTreasuryAdminUpdate inputDatum outputDatum sigs validRange wdrl =
  let SettingsDatumD
        { sdSettingsAdminD = inSettingsAdmin
        , sdMetadataAdminD = inMetadataAdmin
        , sdTreasuryAdminD = inTreasuryAdmin
        , sdAuthorizedScoopersD = inAuthorizedScoopers
        , sdBaseFeeD = inBaseFee
        , sdSimpleFeeD = inSimpleFee
        , sdStrategyFeeD = inStrategyFee
        , sdPoolCreationFeeD = inPoolCreationFee
        , sdExtensionsD = inExtensions
        } = inputDatum
      SettingsDatumD
        { sdSettingsAdminD = outSettingsAdmin
        , sdMetadataAdminD = outMetadataAdmin
        , sdTreasuryAdminD = outTreasuryAdmin
        , sdAuthorizedScoopersD = outAuthorizedScoopers
        , sdBaseFeeD = outBaseFee
        , sdSimpleFeeD = outSimpleFee
        , sdStrategyFeeD = outStrategyFee
        , sdPoolCreationFeeD = outPoolCreationFee
        , sdExtensionsD = outExtensions
        } = outputDatum
      admin = unsafeFromBuiltinData @MultisigScriptD inTreasuryAdmin
      signedByAdmin = multisigSatisfied admin sigs validRange wdrl
      settingsAdminUnchanged = inSettingsAdmin == outSettingsAdmin
      metadataAdminUnchanged = inMetadataAdmin == outMetadataAdmin
      treasuryAdminUnchanged = inTreasuryAdmin == outTreasuryAdmin
      scoopersUnchanged = inAuthorizedScoopers == outAuthorizedScoopers
      baseFeeUnchanged = inBaseFee == outBaseFee
      simpleFeeUnchanged = inSimpleFee == outSimpleFee
      strategyFeeUnchanged = inStrategyFee == outStrategyFee
      poolCreationFeeUnchanged = inPoolCreationFee == outPoolCreationFee
      extensionsUnchanged = inExtensions == outExtensions
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
settingsValidator :: SettingsDatumD -> SettingsRedeemerD -> TxOutRef -> TxInfo -> Bool
settingsValidator inputDatum redeemer ownRef TxInfo{txInfoInputs = inputs, txInfoOutputs = outputs, txInfoMint = mint, txInfoSignatories = sigs, txInfoValidRange = validRange, txInfoWdrl = wdrl} =
  let ownInput = findOwnInput ownRef inputs
      TxInInfo _ ownResolved = ownInput
      TxOut ownAddress ownInputValue _ _ = ownResolved
      ownOutput = DList.head outputs
      TxOut ownOutputAddress ownOutputValue _ _ = ownOutput
      outputDatum = getOutputDatum ownOutput
      valueNotChanged = valueWithoutLovelace ownOutputValue == valueWithoutLovelace ownInputValue
      noMint = valueIsZero mint
   in ownOutputAddress
        == ownAddress
        && valueNotChanged
        && noMint
        && case redeemer of
          SettingsAdminUpdateD ->
            checkSettingsAdminUpdate inputDatum outputDatum sigs validRange wdrl
          TreasuryAdminUpdateD ->
            checkTreasuryAdminUpdate inputDatum outputDatum sigs validRange wdrl

-- ============================================================================
-- 7. Mint validator
-- ============================================================================

{-# INLINEABLE mintsExactlyOneToken #-}
mintsExactlyOneToken :: CurrencySymbol -> TokenName -> MintValue -> Bool
mintsExactlyOneToken expectedCs expectedTn mv =
  let outerPairs = DMap.toBuiltinList (mintValueToMap mv)
   in Builtins.matchList
        outerPairs
        (const False)
        ( \first rest ->
            Builtins.matchList
              rest
              ( \_ ->
                  unsafeFromBuiltinData (BI.fst first)
                    == expectedCs
                    && let innerPairs = BI.unsafeDataAsMap (BI.snd first)
                        in Builtins.matchList
                             innerPairs
                             (const False)
                             ( \tkPair tkRest ->
                                 Builtins.matchList
                                   tkRest
                                   ( \_ ->
                                       unsafeFromBuiltinData (BI.fst tkPair)
                                         == expectedTn
                                         && unsafeFromBuiltinData @Integer (BI.snd tkPair)
                                         == 1
                                   )
                                   (\_ _ -> False)
                             )
              )
              (\_ _ -> False)
        )

{-# INLINEABLE findSettingsOutput #-}
findSettingsOutput :: CurrencySymbol -> DList.List TxOut -> Bool
findSettingsOutput ownPolicyId =
  DList.any
    ( \(TxOut addr _ d _) -> case addressCredential addr of
        ScriptCredential sh ->
          sh
            == ScriptHash (unCurrencySymbol ownPolicyId)
            && (case d of OutputDatum _ -> True; _ -> False)
        _ -> False
    )

{-# INLINEABLE settingsMintValidator #-}
settingsMintValidator :: TxOutRef -> CurrencySymbol -> TxInfo -> Bool
settingsMintValidator bootUtxo ownPolicyId tx =
  let TxInfo{txInfoMint = mint, txInfoInputs = inputs, txInfoOutputs = outputs} = tx
      mintsExactlyOne =
        mintsExactlyOneToken ownPolicyId (TokenName "settings") mint
      spendsBootUtxo =
        DList.any (\inp -> txInInfoOutRef inp == bootUtxo) inputs
      paysToSettingsScript =
        findSettingsOutput ownPolicyId outputs
   in mintsExactlyOne
        && spendsBootUtxo
        && paysToSettingsScript

-- ============================================================================
-- 8. Entry point and compiled script
-- ============================================================================

{-# INLINEABLE mkSettingsValidator #-}
mkSettingsValidator :: BuiltinData -> BuiltinData -> ()
mkSettingsValidator bootUtxoData ctxData =
  let ctx = unsafeFromBuiltinData @ScriptContext ctxData
      ScriptContext{scriptContextTxInfo = txInfo, scriptContextRedeemer = redeemer, scriptContextScriptInfo = scriptInfo} = ctx
   in case scriptInfo of
        SpendingScript ownRef (Just (Datum datumData)) ->
          let datum = unsafeFromBuiltinData @SettingsDatumD datumData
              red = unsafeFromBuiltinData @SettingsRedeemerD (getRedeemer redeemer)
           in if settingsValidator datum red ownRef txInfo then () else error ()
        MintingScript ownPolicyId ->
          let bootUtxo = unsafeFromBuiltinData @TxOutRef bootUtxoData
           in if settingsMintValidator bootUtxo ownPolicyId txInfo then () else error ()
        _ -> error ()

plinthSettingsScript :: Script
plinthSettingsScript =
  compiledCodeToScript $ plinthc mkSettingsValidator
