{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E29Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude

-- BUG: a typed-TH splice that captures the runtime parameter `x`. The
-- plugin compiles the splice body in isolation, walks GHC Core, and
-- reports the free reference to `x`.
mkAdder :: Integer -> CompiledCode (Integer -> Integer)
mkAdder x = $$(compile [||\y -> x + y||])
