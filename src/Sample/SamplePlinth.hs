{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}

module Sample.SamplePlinth where

import Plinth.Plugin
import PlutusTx.AsData (asData)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude

asData
  [d|
    data Withdraw
      = Amount Integer
      | Joint Withdraw Withdraw
      | Deduct Integer Withdraw
      deriving newtype (UnsafeFromData, ToData)
    |]

netWithdraw :: Withdraw -> Integer
netWithdraw w = case w of
  Amount n -> n
  Joint x y -> netWithdraw x + netWithdraw y
  Deduct n from -> netWithdraw from - n

netWithdrawUPLC :: CompiledCode (Withdraw -> Integer)
netWithdrawUPLC = plinthc netWithdraw
