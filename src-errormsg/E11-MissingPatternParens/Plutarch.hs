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

module E11Plutarch where

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

-- BUG: missing parens around (PAmount n) — parsed as two separate arguments
isAmount :: PWithdraw s -> Term s PBool
isAmount PAmount _ = pcon PTrue
isAmount _ = pcon PFalse
