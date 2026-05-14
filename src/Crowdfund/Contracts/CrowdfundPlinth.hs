{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:inline-callsite-growth=20 #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:inline-unconditional-growth=20 #-}

module Crowdfund.Contracts.CrowdfundPlinth (
  plinthCrowdfundScript,
) where

import Plutarch.Script (Script (..))
import PlutusLedgerApi.Data.V3
import PlutusLedgerApi.V1.Data.Interval (matchExtended, matchInterval, matchLowerBound)
import PlutusLedgerApi.V2.Data.Tx (matchOutputDatum)

import Plinth.Plugin
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Code (getPlcNoAnn)
import PlutusTx.Data.AssocMap qualified as DMap
import PlutusTx.Data.List qualified as DList
import PlutusTx.Prelude
import UntypedPlutusCore qualified as UPLC

import Crowdfund.Types.CrowdfundState

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
findOwnInput :: TxOutRef -> DList.List TxInInfo -> TxInInfo
findOwnInput ref inputs =
  case DList.find (\inp -> txInInfoOutRef inp == ref) inputs of
    Just x -> x
    Nothing -> error ()

{-# INLINEABLE getInputsByAddress #-}
getInputsByAddress :: DList.List TxInInfo -> Address -> DList.List TxInInfo
getInputsByAddress inputs addr =
  DList.filter (\inp -> txOutAddress (txInInfoResolved inp) == addr) inputs

{-# INLINEABLE getOutputsByAddress #-}
getOutputsByAddress :: DList.List TxOut -> Address -> DList.List TxOut
getOutputsByAddress outputs addr =
  DList.filter (\o -> txOutAddress o == addr) outputs

{-# INLINEABLE getLovelaceAmount #-}
getLovelaceAmount :: Value -> Integer
getLovelaceAmount (Value m) =
  let outerPairs = BI.unsafeDataAsMap (toBuiltinData m)
      innerPairs = BI.unsafeDataAsMap (BI.snd (BI.head outerPairs))
   in BI.unsafeDataAsI (BI.snd (BI.head innerPairs))

{-# INLINEABLE getAdaFromInputs #-}
getAdaFromInputs :: DList.List TxInInfo -> Integer
getAdaFromInputs = DList.foldl (\acc inp -> acc + getLovelaceAmount (txOutValue (txInInfoResolved inp))) 0

{-# INLINEABLE getAdaFromOutputs #-}
getAdaFromOutputs :: DList.List TxOut -> Integer
getAdaFromOutputs = DList.foldl (\acc o -> acc + getLovelaceAmount (txOutValue o)) 0

{-# INLINEABLE mustStartBeforeTimeout #-}
mustStartBeforeTimeout :: POSIXTimeRange -> Integer -> Bool
mustStartBeforeTimeout range deadline =
  matchInterval range $ \lb _ub ->
    matchLowerBound lb $ \ext _ ->
      matchExtended ext False (\(POSIXTime t) -> t < deadline) False

{-# INLINEABLE mustBeSignedBy #-}
mustBeSignedBy :: DList.List PubKeyHash -> PubKeyHash -> Bool
mustBeSignedBy sigs pkh = DList.elem pkh sigs

{-# INLINEABLE sumWallets #-}
sumWallets :: DMap.Map PubKeyHash Integer -> Integer
sumWallets m = DMap.foldr (\v acc -> v + acc) 0 m

{-# INLINEABLE mapSize #-}
mapSize :: DMap.Map PubKeyHash Integer -> Integer
mapSize m = DList.length (DMap.keys m)

{-# INLINEABLE walletsExcept #-}
walletsExcept :: PubKeyHash -> DMap.Map PubKeyHash Integer -> DMap.Map PubKeyHash Integer
walletsExcept donor m = DMap.delete donor m

{-# INLINEABLE getOutputDatum #-}
getOutputDatum :: TxOut -> CrowdfundDatumD
getOutputDatum o =
  matchOutputDatum
    (txOutDatum o)
    (error ())
    (\_ -> error ())
    (\(Datum d) -> unsafeFromBuiltinData d)

{-# INLINEABLE listLength #-}
listLength :: DList.List a -> Integer
listLength = DList.length

{-# INLINEABLE listHead #-}
listHead :: (UnsafeFromData a) => DList.List a -> a
listHead = DList.head

-- ============================================================================
-- 3. Donate
-- ============================================================================

{-# INLINEABLE checkDonate #-}
checkDonate
  :: PubKeyHash
  -> Integer
  -> Integer
  -> DMap.Map PubKeyHash Integer
  -> POSIXTimeRange
  -> DList.List PubKeyHash
  -> DList.List TxOut
  -> Integer
  -> Integer
  -> PubKeyHash
  -> Bool
checkDonate recipient goal deadline wallets validRange sigs contractOutputs contractAmount amount donor =
  listLength contractOutputs
    == 1
    && let contractOutput = listHead contractOutputs
           outputDatum = getOutputDatum contractOutput
           CrowdfundDatumD
             { cfRecipientD = outRecipient
             , cfGoalD = outGoal
             , cfDeadlineD = outDeadline
             , cfWalletsD = outputWallets
             } = outputDatum
        in mustStartBeforeTimeout validRange deadline
             && mustBeSignedBy sigs donor
             && getAdaFromOutputs contractOutputs
             == contractAmount
             + amount
             && outRecipient
             == recipient
             && outGoal
             == goal
             && outDeadline
             == deadline
             && toBuiltinData (walletsExcept donor wallets)
             == toBuiltinData (walletsExcept donor outputWallets)
             && maybe
               False
               ( \outputWalletAmount ->
                   maybe
                     (amount == outputWalletAmount)
                     (\previousAmount -> outputWalletAmount == previousAmount + amount)
                     (DMap.lookup donor wallets)
               )
               (DMap.lookup donor outputWallets)

-- ============================================================================
-- 4. Withdraw
-- ============================================================================

{-# INLINEABLE checkWithdraw #-}
checkWithdraw
  :: PubKeyHash
  -> Integer
  -> Integer
  -> POSIXTimeRange
  -> DList.List PubKeyHash
  -> Integer
  -> Bool
checkWithdraw recipient goal deadline validRange sigs contractAmount =
  mustBeSignedBy sigs recipient
    && not (mustStartBeforeTimeout validRange deadline)
    && contractAmount
    >= goal

-- ============================================================================
-- 5. Reclaim
-- ============================================================================

{-# INLINEABLE checkReclaim #-}
checkReclaim
  :: PubKeyHash
  -> Integer
  -> Integer
  -> DMap.Map PubKeyHash Integer
  -> POSIXTimeRange
  -> DList.List PubKeyHash
  -> DList.List TxInInfo
  -> DList.List TxOut
  -> Bool
checkReclaim recipient goal deadline wallets validRange sigs contractInputs contractOutputs =
  let currentSigner = DList.head sigs
   in maybe
        False
        ( \withdrawAmount ->
            not (mustStartBeforeTimeout validRange deadline)
              && if mapSize wallets > 1
                then
                  listLength contractOutputs
                    == 1
                    && let contractOutput = listHead contractOutputs
                           outputDatum = getOutputDatum contractOutput
                           CrowdfundDatumD
                             { cfRecipientD = outRecipient
                             , cfGoalD = outGoal
                             , cfDeadlineD = outDeadline
                             , cfWalletsD = outputWallets
                             } = outputDatum
                        in getAdaFromOutputs contractOutputs
                             == getAdaFromInputs contractInputs
                             - withdrawAmount
                             && outRecipient
                             == recipient
                             && outGoal
                             == goal
                             && outDeadline
                             == deadline
                             && toBuiltinData outputWallets
                             == toBuiltinData (walletsExcept currentSigner wallets)
                else listLength contractOutputs == 0
        )
        (DMap.lookup currentSigner wallets)

-- ============================================================================
-- 6. Main validator
-- ============================================================================

{-# INLINEABLE crowdfundValidator #-}
crowdfundValidator
  :: PubKeyHash
  -> Integer
  -> Integer
  -> DMap.Map PubKeyHash Integer
  -> BuiltinData
  -> TxOutRef
  -> TxInfo
  -> Bool
crowdfundValidator
  recipient
  goal
  deadline
  wallets
  redeemerData
  ownRef
  TxInfo
    { txInfoInputs = inputs
    , txInfoOutputs = outputs
    , txInfoValidRange = validRange
    , txInfoSignatories = sigs
    } =
    let ownInput = findOwnInput ownRef inputs
        contractAddress = txOutAddress (txInInfoResolved ownInput)
        contractInputs = getInputsByAddress inputs contractAddress
        contractOutputs = getOutputsByAddress outputs contractAddress
        contractAmount = sumWallets wallets
     in contractAmount
          == getAdaFromInputs contractInputs
          && matchCrowdfundRedeemerD
            (unsafeFromBuiltinData redeemerData)
            ( \amount donor ->
                checkDonate
                  recipient
                  goal
                  deadline
                  wallets
                  validRange
                  sigs
                  contractOutputs
                  contractAmount
                  amount
                  donor
            )
            (checkWithdraw recipient goal deadline validRange sigs contractAmount)
            (checkReclaim recipient goal deadline wallets validRange sigs contractInputs contractOutputs)

-- ============================================================================
-- 7. Entry point and compiled script
-- ============================================================================

{-# INLINEABLE mkCrowdfundValidator #-}
mkCrowdfundValidator :: BuiltinData -> ()
mkCrowdfundValidator ctxData =
  let ctx = unsafeFromBuiltinData @ScriptContext ctxData
      ScriptContext
        { scriptContextTxInfo = txInfo
        , scriptContextRedeemer = redeemer
        , scriptContextScriptInfo = scriptInfo
        } = ctx
   in case scriptInfo of
        SpendingScript ownRef (Just (Datum datumData)) ->
          let datum = unsafeFromBuiltinData @CrowdfundDatumD datumData
              CrowdfundDatumD
                { cfRecipientD = recipient
                , cfGoalD = goal
                , cfDeadlineD = deadline
                , cfWalletsD = wallets
                } = datum
           in if crowdfundValidator recipient goal deadline wallets (getRedeemer redeemer) ownRef txInfo
                then ()
                else error ()
        _ -> error ()

plinthCrowdfundScript :: Script
plinthCrowdfundScript =
  compiledCodeToScript $ plinthc mkCrowdfundValidator
