{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Crowdfund.Types.CrowdfundState where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift ()
import Plutarch.LedgerApi.AssocMap (KeyGuarantees (..), PMap)
import Plutarch.LedgerApi.V3 (PPubKeyHash)
import Plutarch.Prelude
import PlutusLedgerApi.Data.V3 qualified as DV3
import PlutusLedgerApi.V3 (POSIXTime, PubKeyHash)
import PlutusTx qualified
import PlutusTx.AsData (asData)
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Data.AssocMap qualified as DMap

-- ============================================================================
-- Plinth: CrowdfundDatum
-- ============================================================================

data CrowdfundDatum = CrowdfundDatum
  { cfRecipient :: PubKeyHash
  , cfGoal :: Integer
  , cfDeadline :: POSIXTime
  , cfWallets :: Map.Map PubKeyHash Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''CrowdfundDatum [('CrowdfundDatum, 0)]

-- ============================================================================
-- Plinth: CrowdfundRedeemer
-- ============================================================================

data CrowdfundRedeemer
  = Donate Integer PubKeyHash
  | Withdraw
  | Reclaim
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''CrowdfundRedeemer
  [ ('Donate, 0)
  , ('Withdraw, 1)
  , ('Reclaim, 2)
  ]

-- ============================================================================
-- Plinth (AsData): CrowdfundDatumD
-- ============================================================================

$( asData
     [d|
       data CrowdfundDatumD = CrowdfundDatumD
         { cfRecipientD :: DV3.PubKeyHash
         , cfGoalD :: Integer
         , cfDeadlineD :: Integer
         , cfWalletsD :: DMap.Map DV3.PubKeyHash Integer
         }
         deriving newtype (PlutusTx.UnsafeFromData, PlutusTx.ToData)
       |]
 )

-- ============================================================================
-- Plinth (AsData): CrowdfundRedeemerD
-- ============================================================================

$( asData
     [d|
       data CrowdfundRedeemerD
         = DonateD Integer DV3.PubKeyHash
         | WithdrawD
         | ReclaimD
         deriving newtype (PlutusTx.UnsafeFromData)
       |]
 )

-- ============================================================================
-- Plutarch: PCrowdfundDatum
-- ============================================================================

data PCrowdfundDatum (s :: S) = PCrowdfundDatum
  { pcfRecipient :: Term s (PAsData PPubKeyHash)
  , pcfGoal :: Term s (PAsData PInteger)
  , pcfDeadline :: Term s (PAsData PInteger)
  , pcfWallets :: Term s (PAsData (PMap 'Unsorted PPubKeyHash PInteger))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCrowdfundDatum)

deriving via
  DeriveDataPLiftable PCrowdfundDatum CrowdfundDatum
  instance
    PLiftable PCrowdfundDatum

-- ============================================================================
-- Plutarch: PCrowdfundRedeemer
-- ============================================================================

data PCrowdfundRedeemer (s :: S)
  = PDonate
      { pdonateAmount :: Term s (PAsData PInteger)
      , pdonateDonor :: Term s (PAsData PPubKeyHash)
      }
  | PWithdraw
  | PReclaim
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCrowdfundRedeemer)

deriving via
  DeriveDataPLiftable PCrowdfundRedeemer CrowdfundRedeemer
  instance
    PLiftable PCrowdfundRedeemer
