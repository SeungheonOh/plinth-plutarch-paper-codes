{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module SmartTokens.Types.ProtocolParams (
  -- * Plinth type
  ProgrammableLogicGlobalParams (..),

  -- * Plutarch type
  PProgrammableLogicGlobalParams (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift ()
import Plutarch.LedgerApi.V3 (PCredential, PCurrencySymbol)
import Plutarch.Prelude
import PlutusLedgerApi.V3 (Credential, CurrencySymbol)
import PlutusTx.IsData (makeIsDataAsList)

-- ============================================================================
-- Plinth type
-- ============================================================================

data ProgrammableLogicGlobalParams = ProgrammableLogicGlobalParams
  { directoryNodeCS :: CurrencySymbol
  , progLogicCred :: Credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

makeIsDataAsList ''ProgrammableLogicGlobalParams

-- ============================================================================
-- Plutarch type
-- ============================================================================

data PProgrammableLogicGlobalParams (s :: S)
  = PProgrammableLogicGlobalParams
  { pdirectoryNodeCS :: Term s (PAsData PCurrencySymbol)
  , pprogLogicCred :: Term s (PAsData PCredential)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataRec PProgrammableLogicGlobalParams)

deriving via
  DeriveDataPLiftable (PAsData PProgrammableLogicGlobalParams) ProgrammableLogicGlobalParams
  instance
    PLiftable PProgrammableLogicGlobalParams
