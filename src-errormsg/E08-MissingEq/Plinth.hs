{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module E08Plinth where

import PlutusTx.Prelude

-- BUG: missing (Eq a) constraint — uses == on a polymorphic type
isEqual :: a -> a -> Bool
isEqual x y = x == y
