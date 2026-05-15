{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}

module E31Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude

-- No INLINEABLE pragma — without it (and with -fomit-interface-pragmas),
-- GHC will not emit the unfolding into the interface file
doubleIt :: Integer -> Integer
doubleIt n = n + n

-- BUG: `doubleIt` is missing the {-# INLINEABLE doubleIt #-} pragma that
-- Plinth requires on every compiled definition — the plugin reports a
-- reference to a name that is not a local, a builtin, or an external
-- INLINEABLE name
compiledDoubleIt :: CompiledCode (Integer -> Integer)
compiledDoubleIt = $$(compile [||doubleIt||])
