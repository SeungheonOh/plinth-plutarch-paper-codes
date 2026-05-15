{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}

module E30Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude

{-# NOINLINE doubleIt #-}
doubleIt :: Integer -> Integer
doubleIt n = n + n

-- BUG: `doubleIt` is marked NOINLINE, so GHC drops its unfolding from
-- the interface — the Plinth plugin can no longer find the definition
-- it needs to translate to Plutus IR
compiledDoubleIt :: CompiledCode (Integer -> Integer)
compiledDoubleIt = $$(compile [||doubleIt||])
