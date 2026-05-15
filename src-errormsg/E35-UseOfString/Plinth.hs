{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E35Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude
import qualified Prelude as Haskell

{-# INLINEABLE greet #-}
greet :: Haskell.String -> Haskell.String
greet name = "Hello, " Haskell.++ name

-- BUG: Haskell's `String` is `[Char]`, and `Char` is not a Plinth-supported
-- type — on-chain text must use `BuiltinString` and `appendString`, not the
-- base `String`/`++` pair, which has no compilable unfolding
compiledGreet :: CompiledCode (Haskell.String -> Haskell.String)
compiledGreet = $$(compile [||greet||])
