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

module E25Plutarch where

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

pfoldList :: Term s ((b :--> a :--> b) :--> b :--> PList a :--> b)
pfoldList = pfix #$ plam $ \self f acc xs ->
  pmatch xs $ \case
    PNil -> acc
    PCons x rest -> self # f # (f # acc # x) # rest

-- BUG: x (element) is used as the list argument to pfoldList,
-- and rest (PList) is used as the element — creates infinite type
pbadFold :: Term s (PList PInteger :--> PInteger)
pbadFold = pfix #$ plam $ \self xs ->
  pmatch xs $ \case
    PNil -> 0
    PCons x rest -> (pfoldList # plam (\acc _ -> acc) # 0 # x) + (self # rest)
