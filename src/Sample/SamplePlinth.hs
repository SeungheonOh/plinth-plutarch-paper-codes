{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module Sample.SamplePlinth where

import PlutusTx.AsData (asData)
import PlutusTx.Prelude

$( asData
     [d|
       data MaybeData a
         = DNothing
         | DJust a
       |]
 )

data List a
  = Nil
  | Cons a (List a)

fromMaybeData :: (ToData a, UnsafeFromData a) => a -> MaybeData a -> a
fromMaybeData def DNothing = def
fromMaybeData _ (DJust x) = x

foldList :: (b -> a -> b) -> b -> List a -> b
foldList _ acc Nil = acc
foldList f acc (Cons x rest) = foldList f (f acc x) rest

sumMaybeIntegers :: List (MaybeData Integer) -> Integer
sumMaybeIntegers =
  foldList
    ( \acc mx ->
        case mx of
          DNothing -> acc
          DJust n -> acc + n
    )
    0
