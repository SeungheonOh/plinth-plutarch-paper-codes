{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module E14Plinth where

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

-- BUG: modul instead of modulo
netWithdraw :: Withdraw -> Integer
netWithdraw w = case w of
  Amount n -> n `modul` 2
  Joint x y -> netWithdraw x + netWithdraw y
  Deduct n from -> netWithdraw from - n
