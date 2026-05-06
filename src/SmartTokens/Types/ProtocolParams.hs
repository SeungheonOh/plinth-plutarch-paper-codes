{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module SmartTokens.Types.ProtocolParams (
  -- * Plinth type
  ProgrammableLogicGlobalParams (..),

  -- * Plinth (AsData) type
  ProgrammableLogicGlobalParamsD,
  directoryNodeCSD,
  progLogicCredD,

  -- * Plutarch type
  PProgrammableLogicGlobalParams (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift ()
import Plutarch.LedgerApi.V3 (PCredential, PCurrencySymbol)
import Plutarch.Prelude
import PlutusLedgerApi.Data.V3 qualified as DV3
import PlutusLedgerApi.V3 (Credential, CurrencySymbol)
import PlutusTx qualified
import PlutusTx.AsData (asData)
import PlutusTx.Builtins (BuiltinData)

-- ============================================================================
-- Plinth type
-- ============================================================================

data ProgrammableLogicGlobalParams = ProgrammableLogicGlobalParams
  { directoryNodeCS :: CurrencySymbol
  , progLogicCred :: Credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''ProgrammableLogicGlobalParams [('ProgrammableLogicGlobalParams, 0)]

-- ============================================================================
-- Plinth (AsData): ProgrammableLogicGlobalParamsD
-- ============================================================================

$( asData
     [d|
       data ProgrammableLogicGlobalParamsD = ProgrammableLogicGlobalParamsD
         { directoryNodeCSD :: DV3.CurrencySymbol
         , progLogicCredD :: DV3.Credential
         }
         deriving newtype (PlutusTx.UnsafeFromData, PlutusTx.ToData)
       |]
 )

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
  deriving (PlutusType) via (DeriveAsDataStruct PProgrammableLogicGlobalParams)

deriving via
  DeriveDataPLiftable PProgrammableLogicGlobalParams ProgrammableLogicGlobalParams
  instance
    PLiftable PProgrammableLogicGlobalParams
