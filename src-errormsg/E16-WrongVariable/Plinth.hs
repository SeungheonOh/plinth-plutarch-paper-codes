{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module E16Plinth where

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

-- BUG: references 'amount' in Deduct branch, which is not in scope (meant: n)
netWithdraw :: Withdraw -> Integer
netWithdraw w = case w of
  Amount n -> n
  Joint x y -> netWithdraw x + netWithdraw y
  Deduct n from -> netWithdraw from - amount
