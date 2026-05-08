{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module E23Plinth where

import PlutusTx.Prelude

data List a
  = Nil
  | Cons a (List a)

-- BUG: wildcard equation comes first, making the Cons case unreachable
foldList :: (b -> a -> b) -> b -> List a -> b
foldList _ acc _ = acc
foldList f acc (Cons x rest) = foldList f (f acc x) rest
