{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E34Plinth where

import Plinth.Plugin (plinthc)
import qualified PlutusTx
import PlutusTx.Prelude
import System.IO (putStrLn)
import System.IO.Unsafe (unsafePerformIO)
import qualified Prelude as Haskell

-- BUG: embeds an `IO` action inside otherwise-pure code via
-- `unsafePerformIO`. The Plinth plugin's typecheck-stage
-- `injectUnsupportedMarkers` pass spots the `IO`-typed sub-expression
-- and emits its custom diagnostic: "IO actions are not supported in
-- Plinth".
{-# INLINEABLE addWithLog #-}
addWithLog :: Integer -> Integer -> Integer
addWithLog x y = x + unsafePerformIO (putStrLn "computing" Haskell.>> Haskell.pure y)

compiledAddWithLog :: PlutusTx.CompiledCode (Integer -> Integer -> Integer)
compiledAddWithLog = plinthc addWithLog
