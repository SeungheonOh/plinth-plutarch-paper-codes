{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wall -fplugin PlutusTx.Plugin #-}

module E18Plinth where

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

-- BUG: Deduct case is missing — non-exhaustive patterns
netWithdraw :: Withdraw -> Integer
netWithdraw w = case w of
  Amount n -> n
  Joint x y -> netWithdraw x + netWithdraw y
