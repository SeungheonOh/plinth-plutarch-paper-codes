{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module E26Plinth where

import PlutusTx.Prelude

-- BUG: intermediate type between toBuiltinData and unsafeFromBuiltinData is ambiguous
roundtrip :: BuiltinData -> Integer
roundtrip d = toBuiltinData (unsafeFromBuiltinData d)
