{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E32Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude

class HasDefault a where
  type DefaultOf a
  defaultVal :: DefaultOf a

instance HasDefault Integer where
  type DefaultOf Integer = Integer
  defaultVal = 0

{-# INLINEABLE addDefault #-}
addDefault :: forall a. (HasDefault a, DefaultOf a ~ Integer) => Integer -> Integer
addDefault n = defaultVal @a + n

-- BUG: associated type families aren't supported by the Plinth plugin —
-- the equality constraint `DefaultOf a ~ Integer` requires the plugin to
-- reduce a type family through a class dictionary, which it cannot do
compiledAddDefault :: CompiledCode (Integer -> Integer)
compiledAddDefault = $$(compile [||addDefault @Integer||])
