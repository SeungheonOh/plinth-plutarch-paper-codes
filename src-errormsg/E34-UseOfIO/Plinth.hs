{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}

module E34Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude
import System.IO (putStrLn)
import qualified Prelude as Haskell

{-# INLINEABLE logIt #-}
logIt :: Integer -> Haskell.IO ()
logIt n = putStrLn (Haskell.show n)

-- BUG: IO has no representation in Plutus IR — the plugin cannot
-- compile any IO action (and `putStrLn`/`show` would fail on their own
-- as well, both being base methods without on-chain equivalents)
compiledLog :: CompiledCode (Integer -> Haskell.IO ())
compiledLog = $$(compile [||logIt||])
