{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:preserve-source-locations #-}

module E09Plinth where

import PlutusTx.Prelude

-- BUG: missing (Ord a) constraint — uses < and > on a polymorphic type
clamp :: a -> a -> a -> a
clamp lo hi x =
  if x < lo
    then lo
    else
      if x > hi
        then hi
        else x
