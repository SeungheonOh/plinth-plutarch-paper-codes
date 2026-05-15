{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E41Plinth where

import Data.Text (Text)
import qualified Data.Text as Text
import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude

-- BUG: signature uses `Data.Text.Text` instead of `BuiltinString`.
-- Plinth's `unsupportedTypes` table catches `Text` and emits a custom
-- message pointing the user at `BuiltinString` as the on-chain string
-- type.
{-# INLINEABLE greet #-}
greet :: Text -> Text
greet name = "Hello, " `Text.append` name

compiledGreet :: CompiledCode (Text -> Text)
compiledGreet = $$(compile [||greet||])
