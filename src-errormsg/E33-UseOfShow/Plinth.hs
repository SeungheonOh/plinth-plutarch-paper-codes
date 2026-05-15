{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E33Plinth where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude
import qualified Prelude as Haskell

{-# INLINEABLE renderInteger #-}
renderInteger :: Integer -> Haskell.String
renderInteger n = Haskell.show n

-- BUG: `Haskell.show` is a method of base's Show class — it has no
-- unfolding the Plinth plugin can compile, and the result type
-- `String = [Char]` is also unsupported on-chain
compiledRender :: CompiledCode (Integer -> Haskell.String)
compiledRender = $$(compile [||renderInteger||])
