{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E40Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- BUG: function uses `Double` for a percentage calculation. Plinth's
-- `unsupportedTypes` table catches `Double` and emits a custom
-- message pointing the user at `PlutusTx.Ratio.Rational`.
{-# INLINEABLE percent #-}
percent :: Haskell.Double -> Haskell.Double -> Haskell.Double
percent part whole = (part Haskell.* 100.0) Haskell./ whole

compiledPercent :: CompiledCode (Haskell.Double -> Haskell.Double -> Haskell.Double)
compiledPercent = $$(compile [||percent||])
