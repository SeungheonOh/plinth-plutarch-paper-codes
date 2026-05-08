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

module E03Plutarch where

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

psumIntegers :: Term s (PList PInteger :--> PInteger)
psumIntegers = pfoldList # plam (\acc n -> acc + n) # 0

-- BUG: passing a bare PInteger (42) where PList PInteger is expected
result :: Term s PInteger
result = psumIntegers # (42 :: Term s PInteger)
