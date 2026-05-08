{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module E01Plinth where

import PlutusTx.Prelude

data List a
  = Nil
  | Cons a (List a)

foldList :: (b -> a -> b) -> b -> List a -> b
foldList _ acc Nil = acc
foldList f acc (Cons x rest) = foldList f (f acc x) rest

-- BUG: returns Cons (acc + n) Nil (a List) instead of acc + n (an Integer)
sumIntegers :: List Integer -> Integer
sumIntegers = foldList (\acc n -> Cons (acc + n) Nil) 0
