{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:inline-callsite-growth=20 #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:inline-unconditional-growth=20 #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:no-preserve-logging #-}

module Vesting.Contracts.VestingPlinth (
  plinthVestingScript,
) where

import Plinth.Plugin
import Plutarch.Script (Script (..))
import PlutusLedgerApi.Data.V3
import PlutusLedgerApi.V1.Data.Interval (matchExtended, matchInterval, matchLowerBound)
import PlutusLedgerApi.V2.Data.Tx (matchOutputDatum)
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Code (getPlcNoAnn)
import PlutusTx.Data.AssocMap qualified as DAssocMap
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
  case DList.find (\inp -> txInInfoOutRef inp == ref) inputs of
    Just x -> x
    Nothing -> error ()

{-# INLINEABLE getOutputsByAddress #-}
getOutputsByAddress :: List TxOut -> Address -> List TxOut
getOutputsByAddress outputs addr =
  DList.filter (\o -> txOutAddress o == addr) outputs

{- | Lovelace amount from the raw `BuiltinData` representation of a `Value`.
  ADA is the first entry in a sorted value map.
-}
{-# INLINEABLE getLovelaceFromData #-}
getLovelaceFromData :: BuiltinData -> Integer
getLovelaceFromData valData =
  let outerPairs = BI.unsafeDataAsMap valData
      innerPairs = BI.unsafeDataAsMap (BI.snd (BI.head outerPairs))
   in BI.unsafeDataAsI (BI.snd (BI.head innerPairs))

{- | Lovelace amount from a typed `Value`. Uses `toBuiltinList` (O(1) coerce)
  to skip the wasteful `mkMap` + `unsafeDataAsMap` round-trip that
  `toBuiltinData m + unsafeDataAsMap` would otherwise incur.
-}
{-# INLINEABLE getLovelaceAmount #-}
getLovelaceAmount :: Value -> Integer
getLovelaceAmount (Value m) =
  let outerPairs = DAssocMap.toBuiltinList m
      innerPairs = BI.unsafeDataAsMap (BI.snd (BI.head outerPairs))
   in BI.unsafeDataAsI (BI.snd (BI.head innerPairs))

{- | Check whether a raw `Address` data value is `PubKeyCredential vkh`.
  Combines the Address and Credential constructor inspection into one
  pass over the raw data, avoiding two nested asData pattern matches.
-}
{-# INLINEABLE addrIsPkh #-}
addrIsPkh :: BuiltinData -> PubKeyHash -> Bool
addrIsPkh addrData vkh =
  let credData = BI.head (BI.snd (BI.unsafeDataAsConstr addrData))
      credPair = BI.unsafeDataAsConstr credData
   in BI.fst credPair
        == 0
        && BI.unsafeDataAsB (BI.head (BI.snd credPair))
        == getPubKeyHash vkh

{- | Single-pass fold: sum ADA of outputs whose address is a `PubKeyCredential`
  for `vkh`. The inner body works on raw `BuiltinData` so each output is
  inspected in one structural pass instead of four nested asData decodes
  (TxOut -> Address -> Credential, plus the unused datum/refScript fields).
-}
{-# INLINEABLE foldAdaByVkhOutputs #-}
foldAdaByVkhOutputs :: List TxOut -> PubKeyHash -> Integer
foldAdaByVkhOutputs outputs vkh =
  DList.foldl
    ( \acc out ->
        let outFields = BI.snd (BI.unsafeDataAsConstr (toBuiltinData out))
            addrData = BI.head outFields
         in if addrIsPkh addrData vkh
              then acc + getLovelaceFromData (BI.head (BI.tail outFields))
              else acc
    )
    0
    outputs

{-# INLINEABLE foldAdaByVkhInputs #-}
foldAdaByVkhInputs :: List TxInInfo -> PubKeyHash -> Integer
foldAdaByVkhInputs inputs vkh =
  DList.foldl
    ( \acc inp ->
        let resolvedData = BI.head (BI.tail (BI.snd (BI.unsafeDataAsConstr (toBuiltinData inp))))
            outFields = BI.snd (BI.unsafeDataAsConstr resolvedData)
            addrData = BI.head outFields
         in if addrIsPkh addrData vkh
              then acc + getLovelaceFromData (BI.head (BI.tail outFields))
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
    (error ())
    (\_ -> error ())
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
vestingValidator
  beneficiary
  startTimestamp
  duration
  amount
  datumData
  (Release declaredAmount)
  ownRef
  TxInfo
    { txInfoSignatories = sigs
    , txInfoValidRange = validRange
    , txInfoFee = fee
    , txInfoInputs = inputs
    , txInfoOutputs = outputs
    } =
    if mustBeSignedBy sigs beneficiary
      && let ownInput = findOwnInput ownRef inputs
             ownResolved = txInInfoResolved ownInput
             TxOut ownValue ownAddress _ _ = ownResolved
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
      ScriptContext
        { scriptContextTxInfo = txInfo
        , scriptContextRedeemer = redeemer
        , scriptContextScriptInfo = scriptInfo
        } = ctx
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
           in vestingValidator
                beneficiary
                startTimestamp
                duration
                amount
                datumData
                red
                ownRef
                txInfo
        _ -> error ()

plinthVestingScript :: Script
plinthVestingScript =
  compiledCodeToScript $ plinthc mkVestingValidator
