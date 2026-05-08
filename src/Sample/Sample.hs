{-# OPTIONS_GHC -Wno-missing-deriving-strategies -Wno-missing-import-lists -Wno-missing-export-lists #-}

module Sample.Sample where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Term (Config (NoTracing), Script, compile)
import Plutarch.Prelude

data PWithdraw s
  = PAmount (Term s (PAsData PInteger))
  | PJoint (Term s (PAsData PWithdraw)) (Term s (PAsData PWithdraw))
  | PDeduct (Term s (PAsData PInteger)) (Term s (PAsData PWithdraw))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving PlutusType via DeriveAsDataStruct PWithdraw

netWithdraw :: Term s (PWithdraw :--> PInteger)
netWithdraw = pfix #$ plam $ \self wt -> pmatch wt $ \w ->
  case w of
    PAmount n -> pfromData n
    PJoint x y -> (self # pfromData x) + (self # pfromData y)
    PDeduct n from -> (self # pfromData from) - pfromData n

netWithdrawUPLC :: Script
netWithdrawUPLC = case compile NoTracing netWithdraw of
  Right s -> s
  Left err -> error (show err)
