{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}

module E27Plinth where

import Plinth.Plugin (plinthc)
import PlutusTx qualified
import PlutusTx.Prelude

-- BUG: GADT pattern-matched inside a `plinthc`-compiled binding. The
-- plugin's Core->PIR stage rejects the GADTs language extension.
data Tag a where
  TagInt :: Tag Integer
  TagBool :: Tag Bool

useTag :: Tag Integer -> Integer
useTag TagInt = 1

{-# INLINABLE buggyEntry #-}
buggyEntry :: BuiltinData -> ()
buggyEntry _ = if useTag TagInt == 1 then () else error ()

plinthBuggy :: PlutusTx.CompiledCode (BuiltinData -> ())
plinthBuggy = plinthc buggyEntry
