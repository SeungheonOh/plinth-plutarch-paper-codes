{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module Vesting.Contracts.VestingPlinth (
  plinthVestingScript,
) where

import Plutarch.Script (Script (..))
import PlutusLedgerApi.Data.V3
import PlutusLedgerApi.V1.Data.Interval (matchExtended, matchInterval, matchLowerBound)
import PlutusLedgerApi.V2.Data.Tx (matchOutputDatum)

import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Code (getPlcNoAnn)
import PlutusTx.Data.List (List)
import PlutusTx.Data.List qualified as DList
import PlutusTx.Prelude
import UntypedPlutusCore qualified as UPLC

import Vesting.Types.VestingState

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

compiledCodeToScript :: PlutusTx.CompiledCode a -> Script
compiledCodeToScript code =
  let UPLC.Program ann ver body = getPlcNoAnn code
   in Script (UPLC.Program ann ver (UPLC.termMapNames UPLC.unNameDeBruijn body))

-- ============================================================================
-- 2. Utility functions
-- ============================================================================

{-# INLINEABLE findOwnInput #-}
findOwnInput :: TxOutRef -> List TxInInfo -> TxInInfo
findOwnInput ref inputs =
  case DList.find (\(TxInInfo r _) -> r == ref) inputs of
    Just x -> x
    Nothing -> traceError "own input not found"

{-# INLINEABLE getOutputsByAddress #-}
getOutputsByAddress :: List TxOut -> Address -> List TxOut
getOutputsByAddress outputs addr =
  DList.filter (\o -> txOutAddress o == addr) outputs

{-# INLINEABLE credentialMatchesVkh #-}
credentialMatchesVkh :: Address -> PubKeyHash -> Bool
credentialMatchesVkh (Address (PubKeyCredential pkh) _) vkh = pkh == vkh
credentialMatchesVkh _ _ = False

{-# INLINEABLE getOutputsByVkh #-}
getOutputsByVkh :: List TxOut -> PubKeyHash -> List TxOut
getOutputsByVkh outputs vkh =
  DList.filter (\o -> credentialMatchesVkh (txOutAddress o) vkh) outputs

{-# INLINEABLE getInputsByVkh #-}
getInputsByVkh :: List TxInInfo -> PubKeyHash -> List TxInInfo
getInputsByVkh inputs vkh =
  DList.filter (\(TxInInfo _ o) -> credentialMatchesVkh (txOutAddress o) vkh) inputs

{-# INLINEABLE getLovelaceAmount #-}
getLovelaceAmount :: Value -> Integer
getLovelaceAmount (Value m) =
  let outerPairs = BI.unsafeDataAsMap (toBuiltinData m)
      innerPairs = BI.unsafeDataAsMap (BI.snd (BI.head outerPairs))
   in BI.unsafeDataAsI (BI.snd (BI.head innerPairs))

{-# INLINEABLE getAdaFromInputs #-}
getAdaFromInputs :: List TxInInfo -> Integer
getAdaFromInputs = DList.foldl (\acc (TxInInfo _ o) -> acc + getLovelaceAmount (txOutValue o)) 0

{-# INLINEABLE getAdaFromOutputs #-}
getAdaFromOutputs :: List TxOut -> Integer
getAdaFromOutputs = DList.foldl (\acc o -> acc + getLovelaceAmount (txOutValue o)) 0

{-# INLINEABLE foldAdaByVkhOutputs #-}
foldAdaByVkhOutputs :: List TxOut -> PubKeyHash -> Integer
foldAdaByVkhOutputs outputs vkh =
  DList.foldl
    ( \acc (TxOut addr val _ _) ->
        if credentialMatchesVkh addr vkh
          then acc + getLovelaceAmount val
          else acc
    )
    0
    outputs

{-# INLINEABLE foldAdaByVkhInputs #-}
foldAdaByVkhInputs :: List TxInInfo -> PubKeyHash -> Integer
foldAdaByVkhInputs inputs vkh =
  DList.foldl
    ( \acc (TxInInfo _ (TxOut addr val _ _)) ->
        if credentialMatchesVkh addr vkh
          then acc + getLovelaceAmount val
          else acc
    )
    0
    inputs

{-# INLINEABLE mustBeSignedBy #-}
mustBeSignedBy :: List PubKeyHash -> PubKeyHash -> Bool
mustBeSignedBy sigs pkh = DList.elem pkh sigs

{-# INLINEABLE getEarliestTime #-}
getEarliestTime :: POSIXTimeRange -> Integer
getEarliestTime validRange =
  matchInterval validRange $ \lb _ub ->
    matchLowerBound lb $ \ext _ ->
      matchExtended ext 0 (\(POSIXTime t) -> t) 0

{-# INLINEABLE getOutputDatum #-}
getOutputDatum :: TxOut -> VestingDatum
getOutputDatum o =
  matchOutputDatum
    (txOutDatum o)
    (traceError "no inline datum")
    (\_ -> traceError "no inline datum")
    (\(Datum d) -> unsafeFromBuiltinData d)

-- ============================================================================
-- 3. Linear vesting formula
-- ============================================================================

{-# INLINEABLE linearVesting #-}
linearVesting :: Integer -> Integer -> Integer -> Integer -> Integer
linearVesting startTimestamp duration totalAllocation timestamp
  | timestamp < startTimestamp = 0
  | timestamp > startTimestamp + duration = totalAllocation
  | otherwise = divide (totalAllocation * (timestamp - startTimestamp)) duration

-- ============================================================================
-- 4. Main validator logic
-- ============================================================================

{-# INLINEABLE vestingValidator #-}
vestingValidator
  :: PubKeyHash
  -> Integer
  -> Integer
  -> Integer
  -> BuiltinData
  -> VestingRedeemer
  -> TxOutRef
  -> TxInfo
  -> ()
vestingValidator beneficiary startTimestamp duration amount datumData (Release declaredAmount) ownRef TxInfo{txInfoSignatories = sigs, txInfoValidRange = validRange, txInfoFee = fee, txInfoInputs = inputs, txInfoOutputs = outputs} =
  if mustBeSignedBy sigs beneficiary
    && let ownInput = findOwnInput ownRef inputs
           ownResolved = txInInfoResolved ownInput
           TxOut ownAddress ownValue _ _ = ownResolved
           contractAmount = getLovelaceAmount ownValue
           txEarliestTime = getEarliestTime validRange
           released = amount - contractAmount
           releaseAmount =
             linearVesting startTimestamp duration amount txEarliestTime
               - released
        in declaredAmount
             == releaseAmount
             && foldAdaByVkhOutputs outputs beneficiary
             == declaredAmount
             + foldAdaByVkhInputs inputs beneficiary
             - getLovelace fee
             && ( (declaredAmount == contractAmount)
                    || let contractOutputs = getOutputsByAddress outputs ownAddress
                        in DList.length contractOutputs
                             == 1
                             && toBuiltinData (getOutputDatum (DList.head contractOutputs))
                             == datumData
                )
    then ()
    else error ()

-- ============================================================================
-- 5. Entry point
-- ============================================================================

{-# INLINEABLE mkVestingValidator #-}
mkVestingValidator :: BuiltinData -> ()
mkVestingValidator ctxData =
  let ctx = unsafeFromBuiltinData @ScriptContext ctxData
      ScriptContext{scriptContextTxInfo = txInfo, scriptContextRedeemer = redeemer, scriptContextScriptInfo = scriptInfo} = ctx
   in case scriptInfo of
        SpendingScript ownRef (Just (Datum datumData)) ->
          let datum = unsafeFromBuiltinData @VestingDatum datumData
              VestingDatum
                { vdBeneficiary = beneficiary
                , vdStartTimestamp = startTimestamp
                , vdDuration = duration
                , vdAmount = amount
                } = datum
              red = unsafeFromBuiltinData @VestingRedeemer (getRedeemer redeemer)
           in vestingValidator beneficiary startTimestamp duration amount datumData red ownRef txInfo
        _ -> error ()

plinthVestingScript :: Script
plinthVestingScript =
  compiledCodeToScript $$(PlutusTx.compile [||mkVestingValidator||])
