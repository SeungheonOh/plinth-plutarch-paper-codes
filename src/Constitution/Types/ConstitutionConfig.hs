{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Constitution.Types.ConstitutionConfig where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift ()
import Plutarch.Prelude
import PlutusTx qualified

-- ============================================================================
-- Plinth: PredKey
-- ============================================================================

data PredKey = MinValue | MaxValue | NotEqual
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''PredKey
  [('MinValue, 0), ('MaxValue, 1), ('NotEqual, 2)]

-- ============================================================================
-- Plinth: ParamValue
-- ============================================================================

data ParamValue
  = ParamInteger [(PredKey, [Integer])]
  | ParamRational [(PredKey, [(Integer, Integer)])]
  | ParamList [ParamValue]
  | ParamAny
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''ParamValue
  [ ('ParamInteger, 0)
  , ('ParamRational, 1)
  , ('ParamList, 2)
  , ('ParamAny, 3)
  ]

-- ============================================================================
-- Plutarch: PPredKey
-- ============================================================================

data PPredKey (s :: S)
  = PMinValue
  | PMaxValue
  | PNotEqual
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPredKey)

deriving via
  DeriveDataPLiftable PPredKey PredKey
  instance
    PLiftable PPredKey

-- ============================================================================
-- Plutarch: PParamValue
-- ============================================================================

data PParamValue (s :: S)
  = PParamInteger (Term s (PAsData (PBuiltinList PData)))
  | PParamRational (Term s (PAsData (PBuiltinList PData)))
  | PParamList (Term s (PAsData (PBuiltinList PData)))
  | PParamAny
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PParamValue)

deriving via
  DeriveDataPLiftable PParamValue ParamValue
  instance
    PLiftable PParamValue
