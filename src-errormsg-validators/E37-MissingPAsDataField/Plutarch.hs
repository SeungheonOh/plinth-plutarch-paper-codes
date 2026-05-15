{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Vesting.Types.VestingState where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift ()
import Plutarch.LedgerApi.V3 (PPubKeyHash)
import Plutarch.Prelude
import PlutusLedgerApi.V3 (PubKeyHash)
import PlutusTx qualified
import PlutusTx.AsData qualified as PlutusTx

-- ============================================================================
-- Plinth: VestingDatum (data-encoded, newtype over BuiltinData)
-- ============================================================================

PlutusTx.asData
  [d|
    data VestingDatum = VestingDatum
      { vdBeneficiary :: PubKeyHash
      , vdStartTimestamp :: Integer
      , vdDuration :: Integer
      , vdAmount :: Integer
      }
      deriving stock (Show, Eq)
      deriving newtype (PlutusTx.FromData, PlutusTx.UnsafeFromData, PlutusTx.ToData)
    |]

-- ============================================================================
-- Plinth: VestingRedeemer (data-encoded, newtype over BuiltinData)
-- ============================================================================

PlutusTx.asData
  [d|
    data VestingRedeemer = Release Integer
      deriving stock (Show, Eq)
      deriving newtype (PlutusTx.FromData, PlutusTx.UnsafeFromData, PlutusTx.ToData)
    |]

-- ============================================================================
-- Plutarch: PVestingDatum
-- ============================================================================

data PVestingDatum (s :: S) = PVestingDatum
  { pvdBeneficiary :: Term s (PAsData PPubKeyHash)
  , pvdStartTimestamp :: Term s (PAsData PInteger)
  , pvdDuration :: Term s (PAsData PInteger)
  , pvdAmount :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVestingDatum)

deriving via
  DeriveDataPLiftable PVestingDatum VestingDatum
  instance
    PLiftable PVestingDatum

-- ============================================================================
-- Plutarch: PVestingRedeemer
-- ============================================================================

newtype PVestingRedeemer (s :: S) = PRelease (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVestingRedeemer)

deriving via
  DeriveDataPLiftable PVestingRedeemer VestingRedeemer
  instance
    PLiftable PVestingRedeemer
