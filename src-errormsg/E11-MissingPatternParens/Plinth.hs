{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}

module E11Plinth where

import PlutusTx.Prelude

data List a
  = Nil
  | Cons a (List a)

foldList :: (b -> a -> b) -> b -> List a -> b
foldList _ acc Nil = acc
-- BUG: missing parens around (Cons x rest) — parsed as four separate arguments
foldList f acc Cons x rest = foldList f (f acc x) rest
