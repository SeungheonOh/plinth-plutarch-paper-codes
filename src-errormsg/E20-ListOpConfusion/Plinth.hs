{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module E20Plinth where

import PlutusTx.Prelude

data List a
  = Nil
  | Cons a (List a)

-- BUG: Cons expects an element as first arg, but xs (a List) is passed
-- This is like confusing (:) with (++)
prepend :: List a -> List a -> List a
prepend xs ys = Cons xs ys
