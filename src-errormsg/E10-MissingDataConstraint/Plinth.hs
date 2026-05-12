{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}

module E10Plinth where

import PlutusTx.AsData (asData)
import PlutusTx.Prelude

asData
  [d|
    data MaybeData a
      = DNothing
      | DJust a
    |]

-- BUG: missing (ToData a, UnsafeFromData a) constraint
fromMaybeData :: a -> MaybeData a -> a
fromMaybeData def DNothing = def
fromMaybeData _ (DJust x) = x
