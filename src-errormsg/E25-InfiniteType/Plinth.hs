{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E25Plinth where

import PlutusTx.Prelude

data List a
  = Nil
  | Cons a (List a)

foldList :: (b -> a -> b) -> b -> List a -> b
foldList _ acc Nil = acc
foldList f acc (Cons x rest) = foldList f (f acc x) rest

-- BUG: x (element) is used as the list argument to foldList,
-- and rest (List) is used as the element — creates infinite type
badFold :: List Integer -> Integer
badFold Nil = 0
badFold (Cons x rest) = foldList (\acc _ -> acc) 0 x + badFold rest
