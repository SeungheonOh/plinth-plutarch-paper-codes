{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E38Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- BUG: signature uses base's `Int` instead of on-chain `Integer`.
-- The Plinth plugin special-cases `Int` in `compileTyCon` and emits
-- a tight custom message: "Int: use Integer instead".
{-# INLINEABLE addPair #-}
addPair :: Haskell.Int -> Haskell.Int -> Haskell.Int
addPair x y = x Haskell.+ y

compiledAddPair :: CompiledCode (Haskell.Int -> Haskell.Int -> Haskell.Int)
compiledAddPair = $$(compile [||addPair||])
