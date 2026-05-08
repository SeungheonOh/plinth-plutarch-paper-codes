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
{-# OPTIONS_GHC -Wall -Wno-missing-deriving-strategies -Wno-missing-import-lists -Wno-missing-export-lists #-}

module E17Plutarch where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude hiding (PCons, PList, PNil)

data PList (a :: S -> Type) (s :: S)
  = PNil
  | PCons (Term s a) (Term s (PList a))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct (PList a))

-- BUG: PNil base case is missing — non-exhaustive patterns in pmatch handler
pfoldList :: Term s ((b :--> a :--> b) :--> b :--> PList a :--> b)
pfoldList = pfix #$ plam $ \self f acc xs ->
  pmatch xs $ \case
    PCons x rest -> self # f # (f # acc # x) # rest
