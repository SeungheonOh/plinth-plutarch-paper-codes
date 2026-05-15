{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies -Wno-missing-import-lists -Wno-missing-export-lists #-}

module E37Plutarch where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

-- BUG: the `pmd'count` field is `Term s PInteger` but `DeriveAsDataStruct`
-- demands that every field's innermost representation be `PData` (i.e.
-- the field type must be wrapped in `PAsData`). Plutarch enforces this
-- with a `PInnermostIsDataDataRepr` constraint that fires the
-- `PInnermostIsData'` type-family `TypeError`, naming the offending
-- field type in the diagnostic.
data PMyDatum s = PMyDatum
  { pmd'count :: Term s PInteger
  , pmd'flag :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via DeriveAsDataStruct PMyDatum
