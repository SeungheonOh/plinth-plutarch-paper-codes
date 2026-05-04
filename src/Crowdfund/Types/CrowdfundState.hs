{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Crowdfund.Types.CrowdfundState where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift ()
import Plutarch.LedgerApi.AssocMap (KeyGuarantees (..), PMap)
import Plutarch.LedgerApi.V3 (PPubKeyHash)
import Plutarch.Prelude
import PlutusLedgerApi.V3 (POSIXTime, PubKeyHash)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.IsData (makeIsDataAsList)

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

makeIsDataAsList ''CrowdfundDatum

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
  deriving (PlutusType) via (DeriveAsDataRec PCrowdfundDatum)

deriving via
  DeriveDataPLiftable (PAsData PCrowdfundDatum) CrowdfundDatum
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
