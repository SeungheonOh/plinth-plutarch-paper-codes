{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module SmartTokens.Types.ProgrammableLogicGlobal (
  -- * Plinth redeemer type
  ProgrammableLogicGlobalRedeemer (..),

  -- * Offchain helpers
  absoluteToRelativeInputIdxs,
  mkSeizeActRedeemerFromRelativeInputIdxs,
  mkSeizeActRedeemerFromAbsoluteInputIdxs,

  -- * Plutarch redeemer type
  PProgrammableLogicGlobalRedeemer (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift ()
import Plutarch.Prelude
import PlutusTx qualified

-- ============================================================================
-- Plinth redeemer type
-- ============================================================================

data ProgrammableLogicGlobalRedeemer
  = TransferAct
      { plgrTransferProofs :: [Integer]
      , plgrMintProofs :: [Integer]
      }
  | SeizeAct
      { plgrDirectoryNodeIdx :: Integer
      , plgrInputIdxs :: [Integer]
      , plgrOutputsStartIdx :: Integer
      , plgrLengthInputIdxs :: Integer
      }
  deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''ProgrammableLogicGlobalRedeemer
  [('TransferAct, 0), ('SeizeAct, 1)]

-- ============================================================================
-- Plutarch redeemer type
-- ============================================================================

data PProgrammableLogicGlobalRedeemer (s :: S)
  = PTransferAct
      { ptransferProofs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
      , pmintProofs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
      }
  | PSeizeAct
      { pdirectoryNodeIdx :: Term s (PAsData PInteger)
      , pinputIdxs :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
      , poutputsStartIdx :: Term s (PAsData PInteger)
      , plengthInputIdxs :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProgrammableLogicGlobalRedeemer)

deriving via
  DeriveDataPLiftable PProgrammableLogicGlobalRedeemer ProgrammableLogicGlobalRedeemer
  instance
    PLiftable PProgrammableLogicGlobalRedeemer

-- ============================================================================
-- Offchain helpers
-- ============================================================================

absoluteToRelativeInputIdxs :: [Integer] -> [Integer]
absoluteToRelativeInputIdxs [] = []
absoluteToRelativeInputIdxs (firstAbsIdx : remainingAbsIdxs)
  | firstAbsIdx < 0 = error "absoluteToRelativeInputIdxs: negative absolute index"
  | otherwise = firstAbsIdx : go firstAbsIdx remainingAbsIdxs
 where
  go :: Integer -> [Integer] -> [Integer]
  go _ [] = []
  go previousAbsIdx (currentAbsIdx : restAbsIdxs)
    | currentAbsIdx <= previousAbsIdx = error "absoluteToRelativeInputIdxs: absolute indexes must be strictly increasing"
    | otherwise = (currentAbsIdx - previousAbsIdx - 1) : go currentAbsIdx restAbsIdxs

mkSeizeActRedeemerFromRelativeInputIdxs :: Integer -> [Integer] -> Integer -> ProgrammableLogicGlobalRedeemer
mkSeizeActRedeemerFromRelativeInputIdxs directoryNodeIdx relativeInputIdxs outputsStartIdx
  | any (< 0) relativeInputIdxs = error "mkSeizeActRedeemerFromRelativeInputIdxs: negative relative index"
  | otherwise =
      SeizeAct
        { plgrDirectoryNodeIdx = directoryNodeIdx
        , plgrInputIdxs = relativeInputIdxs
        , plgrOutputsStartIdx = outputsStartIdx
        , plgrLengthInputIdxs = fromIntegral (length relativeInputIdxs)
        }

mkSeizeActRedeemerFromAbsoluteInputIdxs :: Integer -> [Integer] -> Integer -> ProgrammableLogicGlobalRedeemer
mkSeizeActRedeemerFromAbsoluteInputIdxs directoryNodeIdx absoluteInputIdxs =
  mkSeizeActRedeemerFromRelativeInputIdxs
    directoryNodeIdx
    (absoluteToRelativeInputIdxs absoluteInputIdxs)
