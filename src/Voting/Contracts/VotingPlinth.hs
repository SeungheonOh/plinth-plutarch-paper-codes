{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Voting.Contracts.VotingPlinth (
  plinthVotingScript,
) where

import Plutarch.Script (Script (..))
import PlutusLedgerApi.Data.V3

import PlutusTx qualified
import PlutusTx.Data.AssocMap qualified as Map
import PlutusTx.Code (getPlcNoAnn)
import PlutusTx.Data.List (List)
import PlutusTx.Data.List qualified as DList
import PlutusTx.Prelude
import UntypedPlutusCore qualified as UPLC

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

{-# INLINEABLE valueHasNFT #-}
valueHasNFT :: CurrencySymbol -> TokenName -> Value -> Bool
valueHasNFT cs tn (Value m) =
  case Map.lookup cs m of
    Just inner -> case Map.lookup tn inner of
      Just qty -> qty == 1
      Nothing -> False
    Nothing -> False

{-# INLINEABLE inputHasNFT #-}
inputHasNFT :: CurrencySymbol -> TokenName -> TxInInfo -> Bool
inputHasNFT cs tn (TxInInfo _ (TxOut _ val _ _)) = valueHasNFT cs tn val

{-# INLINEABLE anyInputHasNFT #-}
anyInputHasNFT :: CurrencySymbol -> TokenName -> List TxInInfo -> Bool
anyInputHasNFT cs tn = DList.any (inputHasNFT cs tn)

-- ============================================================================
-- 3. Main validator logic
-- ============================================================================

{-# INLINEABLE votingValidator #-}
votingValidator :: CurrencySymbol -> TokenName -> TxInfo -> ()
votingValidator cs tn txInfo =
  if anyInputHasNFT cs tn (txInfoInputs txInfo)
    then ()
    else error ()

-- ============================================================================
-- 4. Entry point
-- ============================================================================

{-# INLINEABLE mkVotingValidator #-}
mkVotingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkVotingValidator csData tnData ctxData =
  let cs = unsafeFromBuiltinData @CurrencySymbol csData
      tn = unsafeFromBuiltinData @TokenName tnData
      ctx = unsafeFromBuiltinData @ScriptContext ctxData
      ScriptContext{scriptContextTxInfo = txInfo, scriptContextScriptInfo = scriptInfo} = ctx
   in case scriptInfo of
        VotingScript _ -> votingValidator cs tn txInfo
        _ -> error ()

plinthVotingScript :: Script
plinthVotingScript =
  compiledCodeToScript $$(PlutusTx.compile [||mkVotingValidator||])
