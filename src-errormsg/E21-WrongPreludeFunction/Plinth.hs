{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E21Plinth where

import PlutusTx.Prelude

data List a
  = Nil
  | Cons a (List a)

-- BUG: length expects a BuiltinList, not a custom List type
myLength :: List Integer -> Integer
myLength xs = length xs
