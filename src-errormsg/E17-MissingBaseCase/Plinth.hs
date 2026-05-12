{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wall -fplugin Plinth.Plugin #-}

module E17Plinth where

import PlutusTx.Prelude

data List a
  = Nil
  | Cons a (List a)

-- BUG: Nil base case is missing — non-exhaustive patterns
foldList :: (b -> a -> b) -> b -> List a -> b
foldList f acc (Cons x rest) = foldList f (f acc x) rest
