{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}

module E04Plinth where

import PlutusTx.AsData (asData)
import PlutusTx.Prelude

asData
  [d|
    data Withdraw
      = Amount Integer
      | Joint Withdraw Withdraw
      | Deduct Integer Withdraw
      deriving newtype (UnsafeFromData, ToData)
    |]

-- BUG: using / on Integer instead of divide
netWithdraw :: Withdraw -> Integer
netWithdraw w = case w of
  Amount n -> n / 2
  Joint x y -> netWithdraw x + netWithdraw y
  Deduct n from -> netWithdraw from - n
