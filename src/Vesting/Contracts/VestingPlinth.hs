{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:inline-callsite-growth=20 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:inline-constants #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:inline-fix #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-cse-iterations=8 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations-pir=40 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations-uplc=40 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:relaxed-float-in #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:simplifier-evaluate-builtins #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Vesting.Contracts.VestingPlinth (
  plinthVestingScript,
) where

import Plutarch.Script (Script (..))
import PlutusLedgerApi.Data.V3
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
findOwnInput :: ScriptContext -> TxInInfo
findOwnInput (ScriptContext txInfo _ (SpendingScript ref _)) =
  case DList.find (\(TxInInfo r _) -> r == ref) (txInfoInputs txInfo) of
    Just x -> x
    Nothing -> traceError "own input not found"
findOwnInput _ = traceError "not spending"

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
    ( \acc o ->
        if credentialMatchesVkh (txOutAddress o) vkh
          then acc + getLovelaceAmount (txOutValue o)
          else acc
    )
    0
    outputs

{-# INLINEABLE foldAdaByVkhInputs #-}
foldAdaByVkhInputs :: List TxInInfo -> PubKeyHash -> Integer
foldAdaByVkhInputs inputs vkh =
  DList.foldl
    ( \acc (TxInInfo _ o) ->
        if credentialMatchesVkh (txOutAddress o) vkh
          then acc + getLovelaceAmount (txOutValue o)
          else acc
    )
    0
    inputs

{-# INLINEABLE mustBeSignedBy #-}
mustBeSignedBy :: TxInfo -> PubKeyHash -> Bool
mustBeSignedBy tx pkh = DList.elem pkh (txInfoSignatories tx)

{-# INLINEABLE getEarliestTime #-}
getEarliestTime :: TxInfo -> Integer
getEarliestTime tx =
  case txInfoValidRange tx of
    Interval (LowerBound (Finite (POSIXTime t)) _) _ -> t
    _ -> 0

{-# INLINEABLE getFee #-}
getFee :: TxInfo -> Integer
getFee tx = getLovelace (txInfoFee tx)

{-# INLINEABLE getOutputDatum #-}
getOutputDatum :: TxOut -> VestingDatum
getOutputDatum o =
  case txOutDatum o of
    OutputDatum (Datum d) -> unsafeFromBuiltinData d
    _ -> traceError "no inline datum"

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
vestingValidator :: VestingDatum -> VestingRedeemer -> ScriptContext -> Bool
vestingValidator datum (Release declaredAmount) ctx =
  let tx = scriptContextTxInfo ctx
      beneficiary = vdBeneficiary datum
   in mustBeSignedBy tx beneficiary
        && let ownInput = findOwnInput ctx
               ownResolved = txInInfoResolved ownInput
               contractAmount = getLovelaceAmount (txOutValue ownResolved)
               txEarliestTime = getEarliestTime tx
               released = vdAmount datum - contractAmount
               releaseAmount =
                 linearVesting (vdStartTimestamp datum) (vdDuration datum) (vdAmount datum) txEarliestTime
                   - released
            in declaredAmount
                 == releaseAmount
                 && let fee = getFee tx
                     in foldAdaByVkhOutputs (txInfoOutputs tx) beneficiary
                          == declaredAmount
                          + foldAdaByVkhInputs (txInfoInputs tx) beneficiary
                          - fee
                          && ( (declaredAmount == contractAmount)
                                 || let contractOutputs = getOutputsByAddress (txInfoOutputs tx) (txOutAddress ownResolved)
                                     in DList.length contractOutputs
                                          == 1
                                          && toBuiltinData (getOutputDatum (DList.head contractOutputs))
                                          == toBuiltinData datum
                             )

-- ============================================================================
-- 5. Entry point
-- ============================================================================

{-# INLINEABLE mkVestingValidator #-}
mkVestingValidator :: BuiltinData -> ()
mkVestingValidator ctxData =
  let ctx = unsafeFromBuiltinData @ScriptContext ctxData
   in case scriptContextScriptInfo ctx of
        SpendingScript _ (Just (Datum datumData)) ->
          let datum = unsafeFromBuiltinData @VestingDatum datumData
              redeemer = unsafeFromBuiltinData @VestingRedeemer (getRedeemer (scriptContextRedeemer ctx))
           in if vestingValidator datum redeemer ctx then () else error ()
        _ -> error ()

plinthVestingScript :: Script
plinthVestingScript =
  compiledCodeToScript $$(PlutusTx.compile [||mkVestingValidator||])
