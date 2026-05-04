{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Crowdfund.Contracts.CrowdfundPlinth (
  plinthCrowdfundScript,
) where

import Plutarch.Script (Script (..))
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Code (getPlcNoAnn)
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

{-# INLINEABLE listLength #-}
listLength :: [a] -> Integer
listLength [] = 0
listLength (_ : xs) = 1 + listLength xs

{-# INLINEABLE listHead #-}
listHead :: [a] -> a
listHead (x : _) = x
listHead [] = traceError "empty list"

{-# INLINEABLE listElem #-}
listElem :: (Eq a) => a -> [a] -> Bool
listElem _ [] = False
listElem x (y : ys) = x == y || listElem x ys

-- ============================================================================
-- 2. Utility functions
-- ============================================================================

{-# INLINEABLE findOwnInput #-}
findOwnInput :: ScriptContext -> TxInInfo
findOwnInput (ScriptContext txInfo _ (SpendingScript ref _)) =
  case go (txInfoInputs txInfo) of
    Just x -> x
    Nothing -> traceError "own input not found"
 where
  go [] = Nothing
  go (inp@(TxInInfo r _) : rest) = if r == ref then Just inp else go rest
findOwnInput _ = traceError "not spending"

{-# INLINEABLE getInputsByAddress #-}
getInputsByAddress :: [TxInInfo] -> Address -> [TxInInfo]
getInputsByAddress [] _ = []
getInputsByAddress (inp@(TxInInfo _ o) : rest) addr
  | txOutAddress o == addr = inp : getInputsByAddress rest addr
  | otherwise = getInputsByAddress rest addr

{-# INLINEABLE getOutputsByAddress #-}
getOutputsByAddress :: [TxOut] -> Address -> [TxOut]
getOutputsByAddress [] _ = []
getOutputsByAddress (o : rest) addr
  | txOutAddress o == addr = o : getOutputsByAddress rest addr
  | otherwise = getOutputsByAddress rest addr

{-# INLINEABLE getLovelaceAmount #-}
getLovelaceAmount :: Value -> Integer
getLovelaceAmount v =
  case Map.lookup (CurrencySymbol "") (getValue v) of
    Nothing -> 0
    Just tokenMap ->
      case Map.lookup (TokenName "") tokenMap of
        Nothing -> 0
        Just amt -> amt

{-# INLINEABLE getAdaFromInputs #-}
getAdaFromInputs :: [TxInInfo] -> Integer
getAdaFromInputs [] = 0
getAdaFromInputs (TxInInfo _ o : rest) = getLovelaceAmount (txOutValue o) + getAdaFromInputs rest

{-# INLINEABLE getAdaFromOutputs #-}
getAdaFromOutputs :: [TxOut] -> Integer
getAdaFromOutputs [] = 0
getAdaFromOutputs (o : rest) = getLovelaceAmount (txOutValue o) + getAdaFromOutputs rest

{-# INLINEABLE mustStartBeforeTimeout #-}
mustStartBeforeTimeout :: POSIXTimeRange -> POSIXTime -> Bool
mustStartBeforeTimeout range deadline =
  case ivFrom range of
    LowerBound (Finite t) _ -> t < deadline
    _ -> False

{-# INLINEABLE mustBeSignedBy #-}
mustBeSignedBy :: TxInfo -> PubKeyHash -> Bool
mustBeSignedBy tx pkh = listElem pkh (txInfoSignatories tx)

{-# INLINEABLE sumWallets #-}
sumWallets :: Map.Map PubKeyHash Integer -> Integer
sumWallets m = go (Map.toList m)
 where
  go [] = 0
  go ((_, v) : rest) = v + go rest

{-# INLINEABLE mapSize #-}
mapSize :: Map.Map PubKeyHash Integer -> Integer
mapSize m = listLength (Map.toList m)

{-# INLINEABLE walletsExcept #-}
walletsExcept :: PubKeyHash -> [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)]
walletsExcept _ [] = []
walletsExcept donor ((k, v) : rest)
  | k == donor = walletsExcept donor rest
  | otherwise = (k, v) : walletsExcept donor rest

{-# INLINEABLE getOutputDatum #-}
getOutputDatum :: TxOut -> CrowdfundDatum
getOutputDatum o =
  case txOutDatum o of
    OutputDatum (Datum d) ->
      unsafeFromBuiltinData d
    _ -> traceError "no inline datum"

-- ============================================================================
-- 3. Donate
-- ============================================================================

{-# INLINEABLE checkDonate #-}
checkDonate :: CrowdfundDatum -> TxInfo -> [TxOut] -> Integer -> Integer -> PubKeyHash -> Bool
checkDonate datum tx contractOutputs contractAmount amount donor =
  listLength contractOutputs
    == 1
    && let contractOutput = listHead contractOutputs
           outputDatum = getOutputDatum contractOutput
           outputWallets = cfWallets outputDatum
        in mustStartBeforeTimeout (txInfoValidRange tx) (cfDeadline datum)
             && mustBeSignedBy tx donor
             && getAdaFromOutputs contractOutputs
             == contractAmount
             + amount
             && cfRecipient outputDatum
             == cfRecipient datum
             && cfGoal outputDatum
             == cfGoal datum
             && cfDeadline outputDatum
             == cfDeadline datum
             && walletsExcept donor (Map.toList (cfWallets datum))
             == walletsExcept donor (Map.toList outputWallets)
             && case Map.lookup donor outputWallets of
               Nothing -> False
               Just outputWalletAmount ->
                 case Map.lookup donor (cfWallets datum) of
                   Nothing -> amount == outputWalletAmount
                   Just previousAmount -> outputWalletAmount == previousAmount + amount

-- ============================================================================
-- 4. Withdraw
-- ============================================================================

{-# INLINEABLE checkWithdraw #-}
checkWithdraw :: CrowdfundDatum -> TxInfo -> Integer -> Bool
checkWithdraw datum tx contractAmount =
  mustBeSignedBy tx (cfRecipient datum)
    && not (mustStartBeforeTimeout (txInfoValidRange tx) (cfDeadline datum))
    && contractAmount
    >= cfGoal datum

-- ============================================================================
-- 5. Reclaim
-- ============================================================================

{-# INLINEABLE checkReclaim #-}
checkReclaim :: CrowdfundDatum -> TxInfo -> [TxInInfo] -> [TxOut] -> Bool
checkReclaim datum tx contractInputs contractOutputs =
  let currentSigner = listHead (txInfoSignatories tx)
   in case Map.lookup currentSigner (cfWallets datum) of
        Nothing -> False
        Just withdrawAmount ->
          not (mustStartBeforeTimeout (txInfoValidRange tx) (cfDeadline datum))
            && if mapSize (cfWallets datum) > 1
              then
                listLength contractOutputs
                  == 1
                  && let contractOutput = listHead contractOutputs
                         outputDatum = getOutputDatum contractOutput
                      in getAdaFromOutputs contractOutputs
                           == getAdaFromInputs contractInputs
                           - withdrawAmount
                           && cfRecipient outputDatum
                           == cfRecipient datum
                           && cfGoal outputDatum
                           == cfGoal datum
                           && cfDeadline outputDatum
                           == cfDeadline datum
                           && Map.toList (cfWallets outputDatum)
                           == walletsExcept currentSigner (Map.toList (cfWallets datum))
              else listLength contractOutputs == 0

-- ============================================================================
-- 6. Main validator
-- ============================================================================

{-# INLINEABLE crowdfundValidator #-}
crowdfundValidator :: CrowdfundDatum -> CrowdfundRedeemer -> ScriptContext -> Bool
crowdfundValidator datum redeemer ctx =
  let tx = scriptContextTxInfo ctx
      ownInput = findOwnInput ctx
      contractAddress = txOutAddress (txInInfoResolved ownInput)
      contractInputs = getInputsByAddress (txInfoInputs tx) contractAddress
      contractOutputs = getOutputsByAddress (txInfoOutputs tx) contractAddress
      contractAmount = sumWallets (cfWallets datum)
   in contractAmount
        == getAdaFromInputs contractInputs
        && case redeemer of
          Donate amount donor -> checkDonate datum tx contractOutputs contractAmount amount donor
          Withdraw -> checkWithdraw datum tx contractAmount
          Reclaim -> checkReclaim datum tx contractInputs contractOutputs

-- ============================================================================
-- 7. Entry point and compiled script
-- ============================================================================

{-# INLINEABLE mkCrowdfundValidator #-}
mkCrowdfundValidator :: BuiltinData -> ()
mkCrowdfundValidator ctxData =
  let ctx = unsafeFromBuiltinData @ScriptContext ctxData
   in case scriptContextScriptInfo ctx of
        SpendingScript _ (Just (Datum datumData)) ->
          let datum = unsafeFromBuiltinData @CrowdfundDatum datumData
              redeemer = unsafeFromBuiltinData @CrowdfundRedeemer (getRedeemer (scriptContextRedeemer ctx))
           in if crowdfundValidator datum redeemer ctx then () else error ()
        _ -> error ()

plinthCrowdfundScript :: Script
plinthCrowdfundScript =
  compiledCodeToScript $$(PlutusTx.compile [||mkCrowdfundValidator||])
