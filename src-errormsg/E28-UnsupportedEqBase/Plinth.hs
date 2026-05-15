{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}

module E28Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude
import qualified Prelude as Haskell

data Color = Red | Green | Blue
  deriving stock (Haskell.Eq)

{-# INLINEABLE sameColor #-}
sameColor :: Color -> Color -> Haskell.Bool
sameColor x y = x Haskell.== y

-- BUG: uses base's (==) from Haskell's Eq class —
-- the dictionary method has no unfolding the Plinth plugin can compile,
-- and base's Bool isn't the on-chain Bool either
compiledSameColor :: CompiledCode (Color -> Color -> Haskell.Bool)
compiledSameColor = $$(compile [||sameColor||])
