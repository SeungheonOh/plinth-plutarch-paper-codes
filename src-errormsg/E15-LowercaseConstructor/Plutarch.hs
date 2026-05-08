{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies -Wno-missing-import-lists -Wno-missing-export-lists #-}

module E15Plutarch where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

data PWithdraw s
  = PAmount (Term s (PAsData PInteger))
  | PJoint (Term s (PAsData PWithdraw)) (Term s (PAsData PWithdraw))
  | PDeduct (Term s (PAsData PInteger)) (Term s (PAsData PWithdraw))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via DeriveAsDataStruct PWithdraw

-- BUG: pAmount (wrong case) instead of PAmount — not in scope as constructor
netWithdraw :: Term s (PWithdraw :--> PInteger)
netWithdraw = pfix #$ plam $ \self wt -> pmatch wt $ \w ->
  case w of
    pAmount n -> pfromData n
    PJoint x y -> (self # pfromData x) + (self # pfromData y)
    PDeduct n from -> (self # pfromData from) - pfromData n
