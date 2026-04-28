{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module SmartTokens.Types.ProtocolParams (
  ProgrammableLogicGlobalParams (..),
  PProgrammableLogicGlobalParams (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift ()
import Plutarch.LedgerApi.V3 (PCredential, PCurrencySymbol)
import Plutarch.Prelude
import PlutusLedgerApi.V3 (Credential, CurrencySymbol)
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Prelude qualified as PlutusTxPrelude

data ProgrammableLogicGlobalParams = ProgrammableLogicGlobalParams
  { directoryNodeCS :: CurrencySymbol
  , progLogicCred :: Credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

instance PlutusTx.FromData ProgrammableLogicGlobalParams where
  fromBuiltinData dat = do
    xs <- BI.chooseData dat Nothing Nothing (Just $ BI.unsafeDataAsList dat) Nothing Nothing
    directoryNodeCurrSymb <- PlutusTx.fromBuiltinData $ BI.head xs
    progLogicCred <- PlutusTx.fromBuiltinData $ BI.head $ BI.tail xs
    PlutusTxPrelude.pure PlutusTxPrelude.$ ProgrammableLogicGlobalParams directoryNodeCurrSymb progLogicCred

instance PlutusTx.ToData ProgrammableLogicGlobalParams where
  toBuiltinData ProgrammableLogicGlobalParams{directoryNodeCS, progLogicCred} =
    let directoryNodeCS' = PlutusTx.toBuiltinData directoryNodeCS
        progLogicCred' = PlutusTx.toBuiltinData progLogicCred
     in BI.mkList $ BI.mkCons directoryNodeCS' (BI.mkCons progLogicCred' $ BI.mkNilData BI.unitval)

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
