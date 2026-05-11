{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Constitution.Types.ConstitutionConfig where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift ()
import Plutarch.Prelude
import PlutusTx qualified
import PlutusTx.AsData (asData)
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.Data.List qualified as DList

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
-- Plinth (AsData): PredKeyD
-- ============================================================================

$( asData
     [d|
       data PredKeyD
         = MinValueD
         | MaxValueD
         | NotEqualD
         deriving newtype (PlutusTx.UnsafeFromData)
       |]
 )

-- ============================================================================
-- Plinth (AsData): ParamValueD
--
-- Mirrors `ParamValue` on the wire but keeps every list as a data-encoded
-- `DList.List`, so nothing is forced into a regular Haskell list when the
-- script decodes a value.
-- ============================================================================

$( asData
     [d|
       data ParamValueD
         = ParamIntegerD (DList.List (PredKeyD, DList.List Integer))
         | ParamRationalD (DList.List (PredKeyD, DList.List (Integer, Integer)))
         | ParamListD (DList.List BuiltinData)
         | ParamAnyD
         deriving newtype (PlutusTx.UnsafeFromData)
       |]
 )

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
-- Plutarch: PIntPredEntry  (wire: (PredKey, [Integer]))
-- ============================================================================

data PIntPredEntry (s :: S) = PIntPredEntry
  { pipePredKey :: Term s (PAsData PPredKey)
  , pipeExpected :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PIntPredEntry)

-- ============================================================================
-- Plutarch: PRatPair  (wire: (Integer, Integer))
-- ============================================================================

data PRatPair (s :: S) = PRatPair
  { prpNum :: Term s (PAsData PInteger)
  , prpDen :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRatPair)

-- ============================================================================
-- Plutarch: PRatPredEntry  (wire: (PredKey, [(Integer, Integer)]))
-- ============================================================================

data PRatPredEntry (s :: S) = PRatPredEntry
  { prpePredKey :: Term s (PAsData PPredKey)
  , prpeExpected :: Term s (PAsData (PBuiltinList (PAsData PRatPair)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRatPredEntry)

-- ============================================================================
-- Plutarch: PParamValue
-- ============================================================================

data PParamValue (s :: S)
  = PParamInteger (Term s (PAsData (PBuiltinList (PAsData PIntPredEntry))))
  | PParamRational (Term s (PAsData (PBuiltinList (PAsData PRatPredEntry))))
  | PParamList (Term s (PAsData (PBuiltinList (PAsData PParamValue))))
  | PParamAny
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PParamValue)

deriving via
  DeriveDataPLiftable PParamValue ParamValue
  instance
    PLiftable PParamValue

-- ============================================================================
-- Plutarch: PConfigEntry  (wire: (Integer, ParamValue))
-- ============================================================================

data PConfigEntry (s :: S) = PConfigEntry
  { pceParamId :: Term s (PAsData PInteger)
  , pceParamValue :: Term s (PAsData PParamValue)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PConfigEntry)
