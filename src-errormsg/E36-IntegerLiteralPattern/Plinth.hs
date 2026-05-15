{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E36Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude

-- BUG: GHC desugars Integer-literal patterns to `(==)` from base's
-- `GHC.Classes.Eq`/`GHC.Num.Integer.eqInteger`. The user wrote what
-- looks like a perfectly ordinary `case` on `Integer`, but the Plinth
-- plugin cannot compile the base equality the desugarer inserted —
-- there is no on-chain unfolding for `Eq Integer` from `Prelude`.
-- The plugin emits its custom `integerCaseError`, telling the user to
-- use `PlutusTx.Prelude.(==)` in a guard instead of a literal pattern.
{-# INLINEABLE classify #-}
classify :: Integer -> Integer
classify n = case n of
  0 -> 100
  1 -> 200
  _ -> 300

compiledClassify :: CompiledCode (Integer -> Integer)
compiledClassify = $$(compile [||classify||])
