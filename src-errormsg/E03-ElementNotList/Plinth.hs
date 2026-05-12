{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}

module E03Plinth where

import PlutusTx.Prelude

data List a
  = Nil
  | Cons a (List a)

foldList :: (b -> a -> b) -> b -> List a -> b
foldList _ acc Nil = acc
foldList f acc (Cons x rest) = foldList f (f acc x) rest

sumIntegers :: List Integer -> Integer
sumIntegers = foldList (\acc n -> acc + n) 0

-- BUG: passing a bare Integer (42) where List Integer is expected
result :: Integer
result = sumIntegers 42
