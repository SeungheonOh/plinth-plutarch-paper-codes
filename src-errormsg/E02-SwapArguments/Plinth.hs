{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}

module E02Plinth where

import PlutusTx.Prelude

data List a
  = Nil
  | Cons a (List a)

foldList :: (b -> a -> b) -> b -> List a -> b
foldList _ acc Nil = acc
foldList f acc (Cons x rest) = foldList f (f acc x) rest

-- BUG: arguments to foldList are swapped (initial value and function)
sumIntegers :: List Integer -> Integer
sumIntegers = foldList 0 (\acc n -> acc + n)
