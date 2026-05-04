{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Sample.Sample where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude hiding (PCons, PList, PNil)

-- ============================================================================
-- 1. Data-encoded Maybe
-- ============================================================================

data PMaybeData (a :: S -> Type) (s :: S)
  = PDNothing
  | PDJust (Term s (PAsData a))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct (PMaybeData a))

-- ============================================================================
-- 2. SOP-encoded List
-- ============================================================================

data PList (a :: S -> Type) (s :: S)
  = PNil
  | PCons (Term s a) (Term s (PList a))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct (PList a))

-- ============================================================================
-- 3. Functions
-- ============================================================================

pfromMaybeData :: (PIsData a) => Term s (a :--> PMaybeData a :--> a)
pfromMaybeData = phoistAcyclic $ plam $ \def mx ->
  pmatch mx $ \case
    PDNothing -> def
    PDJust x -> pfromData x

pfoldPList :: Term s ((b :--> a :--> b) :--> b :--> PList a :--> b)
pfoldPList = phoistAcyclic $ pfix #$ plam $ \self f acc xs ->
  pmatch xs $ \case
    PNil -> acc
    PCons x rest -> self # f # (f # acc # x) # rest

psumMaybeIntegers :: Term s (PList (PMaybeData PInteger) :--> PInteger)
psumMaybeIntegers = phoistAcyclic $
  plam $ \xs ->
    pfoldPList
      # plam
        ( \acc mx ->
            pmatch mx $ \case
              PDNothing -> acc
              PDJust n -> acc + pfromData n
        )
      # 0
      # xs
