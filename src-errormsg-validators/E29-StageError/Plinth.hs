{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:inline-callsite-growth=20 #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:inline-unconditional-growth=20 #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:no-preserve-logging #-}

module Voting.Contracts.VotingPlinth (
  plinthVotingScript,
  plinthAdderScript,
) where

import Plinth.Plugin
import Plutarch.Script (Script (..))
import PlutusLedgerApi.Data.V3
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Code (getPlcNoAnn)
import PlutusTx.Data.AssocMap qualified as Map
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

{-# INLINEABLE findTNEntry #-}
findTNEntry
  :: BuiltinData
  -> BI.BuiltinList (BI.BuiltinPair BuiltinData BuiltinData)
  -> Bool
findTNEntry tnData = go
 where
  go tokens =
    Builtins.matchList'
      tokens
      False
      ( \tk rest ->
          if Builtins.equalsData (BI.fst tk) tnData
            then BI.unsafeDataAsI (BI.snd tk) == 1
            else go rest
      )

{-# INLINEABLE findCSEntry #-}
findCSEntry
  :: BuiltinData
  -> BuiltinData
  -> BI.BuiltinList (BI.BuiltinPair BuiltinData BuiltinData)
  -> Bool
findCSEntry csData tnData = go
 where
  go entries =
    Builtins.matchList'
      entries
      False
      ( \pair rest ->
          if Builtins.equalsData (BI.fst pair) csData
            then findTNEntry tnData (BI.unsafeDataAsMap (BI.snd pair))
            else go rest
      )

{-# INLINEABLE valueHasNFT #-}
valueHasNFT :: BuiltinData -> BuiltinData -> Value -> Bool
valueHasNFT csData tnData (Value m) =
  findCSEntry csData tnData (Map.toBuiltinList m)

{-# INLINEABLE inputHasNFT #-}
inputHasNFT :: BuiltinData -> BuiltinData -> TxInInfo -> Bool
inputHasNFT csData tnData (TxInInfo _ (TxOut _ val _ _)) =
  valueHasNFT csData tnData val

{-# INLINEABLE anyInputHasNFT #-}
anyInputHasNFT :: BuiltinData -> BuiltinData -> List TxInInfo -> Bool
anyInputHasNFT csData tnData = DList.any (inputHasNFT csData tnData)

-- ============================================================================
-- 3. Main validator logic
-- ============================================================================

{-# INLINEABLE votingValidator #-}
votingValidator :: BuiltinData -> BuiltinData -> TxInfo -> ()
votingValidator csData tnData txInfo =
  if anyInputHasNFT csData tnData (txInfoInputs txInfo)
    then ()
    else error ()

-- ============================================================================
-- 4. Entry point
-- ============================================================================

{-# INLINEABLE mkVotingValidator #-}
mkVotingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkVotingValidator csData tnData ctxData =
  let ctx = unsafeFromBuiltinData @ScriptContext ctxData
      ScriptContext{scriptContextTxInfo = txInfo, scriptContextScriptInfo = scriptInfo} = ctx
   in case scriptInfo of
        VotingScript _ -> votingValidator csData tnData txInfo
        _ -> error ()

plinthVotingScript :: Script
plinthVotingScript =
  compiledCodeToScript $ plinthc mkVotingValidator

-- BUG: `plinthc` is fed a closure capturing the runtime parameter `x`.
-- The Plinth plugin tries to compile `\y -> x + y` to UPLC, sees that
-- `x` is not a local/builtin/inlinable binding, and aborts with a
-- multi-frame "Context: Compiling code:" stack trace.
{-# INLINABLE mkAdder #-}
mkAdder :: Integer -> PlutusTx.CompiledCode (Integer -> Integer)
mkAdder x = plinthc (\y -> x + y)

plinthAdderScript :: Script
plinthAdderScript = compiledCodeToScript (mkAdder 1)
