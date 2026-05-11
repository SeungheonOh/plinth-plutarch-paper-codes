{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Certifying.Contracts.CertifyingPlinth (
  plinthCertifyingScript,
) where

import Plutarch.Script (Script (..))
import PlutusLedgerApi.Data.V3

import PlutusTx qualified
import PlutusTx.Code (getPlcNoAnn)
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

{-# INLINEABLE isEntirelyAfter #-}
isEntirelyAfter :: POSIXTimeRange -> Integer -> Bool
isEntirelyAfter (Interval (LowerBound ext inclusive) _ub) threshold =
  case ext of
    NegInf -> False
    Finite (POSIXTime t) ->
      if inclusive then threshold < t else threshold <= t
    PosInf -> True

{-# INLINEABLE isDelegateToAbstain #-}
isDelegateToAbstain :: Delegatee -> Bool
isDelegateToAbstain (DelegVote DRepAlwaysAbstain) = True
isDelegateToAbstain _ = False

-- ============================================================================
-- 3. Main validator logic
-- ============================================================================

{-# INLINEABLE certifyingValidator #-}
certifyingValidator :: Integer -> TxCert -> TxInfo -> ()
certifyingValidator expiration cert txInfo =
  case cert of
    TxCertRegStaking _ _ -> ()
    TxCertUnRegStaking _ _ ->
      if isEntirelyAfter (txInfoValidRange txInfo) expiration then () else error ()
    TxCertDelegStaking _ delegatee ->
      if isDelegateToAbstain delegatee then () else error ()
    TxCertRegDeleg _ delegatee _ ->
      if isDelegateToAbstain delegatee then () else error ()
    _ -> error ()

-- ============================================================================
-- 4. Entry point
-- ============================================================================

{-# INLINEABLE mkCertifyingValidator #-}
mkCertifyingValidator :: BuiltinData -> BuiltinData -> ()
mkCertifyingValidator expirationData ctxData =
  let expiration = unsafeFromBuiltinData @Integer expirationData
      ctx = unsafeFromBuiltinData @ScriptContext ctxData
      ScriptContext{scriptContextTxInfo = txInfo, scriptContextScriptInfo = scriptInfo} = ctx
   in case scriptInfo of
        CertifyingScript _idx cert -> certifyingValidator expiration cert txInfo
        _ -> error ()

plinthCertifyingScript :: Script
plinthCertifyingScript =
  compiledCodeToScript $$(PlutusTx.compile [||mkCertifyingValidator||])
