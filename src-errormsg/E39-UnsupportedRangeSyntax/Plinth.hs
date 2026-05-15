{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E39Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude

-- BUG: `[1 .. n]` desugars to `Prelude.enumFromTo 1 n`, which the
-- Plinth plugin rejects with a custom message naming
-- `PlutusTx.Enum.enumFromTo` as the on-chain replacement. The user
-- never types `enumFromTo` — the bug is hidden behind range-syntax.
{-# INLINEABLE upTo #-}
upTo :: Integer -> [Integer]
upTo n = [1 .. n]

compiledUpTo :: CompiledCode (Integer -> [Integer])
compiledUpTo = $$(compile [||upTo||])
