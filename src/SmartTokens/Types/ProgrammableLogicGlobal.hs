{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module SmartTokens.Types.ProgrammableLogicGlobal (
  -- * Plinth redeemer type
  ProgrammableLogicGlobalRedeemer,
  pattern TransferAct,
  pattern SeizeAct,
  plgrTransferProofs,
  plgrMintProofs,
  plgrDirectoryNodeIdx,
  plgrInputIdxs,
  plgrOutputsStartIdx,
  plgrLengthInputIdxs,

  -- * Offchain helpers
  dlistFromList,
  mkTransferAct,
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
import PlutusTx.AsData (asData)
import PlutusTx.Data.List qualified as DList

-- ============================================================================
-- Plinth redeemer type (data-backed via asData)
--
-- The proof / index lists are kept as data-encoded `DList.List Integer` so that
-- the on-chain validator never has to materialize a regular Haskell `[Integer]`.
-- ============================================================================

$( asData
     [d|
       data ProgrammableLogicGlobalRedeemer
         = TransferAct
             { plgrTransferProofs :: DList.List Integer
             , plgrMintProofs :: DList.List Integer
             }
         | SeizeAct
             { plgrDirectoryNodeIdx :: Integer
             , plgrInputIdxs :: DList.List Integer
             , plgrOutputsStartIdx :: Integer
             , plgrLengthInputIdxs :: Integer
             }
         deriving newtype (PlutusTx.UnsafeFromData, PlutusTx.ToData)
       |]
 )

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

-- ============================================================================
-- Offchain helpers
-- ============================================================================

-- | Convert a regular Haskell list to a data-encoded `DList.List`.
dlistFromList :: (PlutusTx.ToData a) => [a] -> DList.List a
dlistFromList = foldr DList.cons DList.nil

-- | Build a `TransferAct` redeemer from regular Haskell lists.
mkTransferAct :: [Integer] -> [Integer] -> ProgrammableLogicGlobalRedeemer
mkTransferAct transferProofs mintProofs =
  TransferAct (dlistFromList transferProofs) (dlistFromList mintProofs)

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
        directoryNodeIdx
        (dlistFromList relativeInputIdxs)
        outputsStartIdx
        (fromIntegral (length relativeInputIdxs))

mkSeizeActRedeemerFromAbsoluteInputIdxs :: Integer -> [Integer] -> Integer -> ProgrammableLogicGlobalRedeemer
mkSeizeActRedeemerFromAbsoluteInputIdxs directoryNodeIdx absoluteInputIdxs =
  mkSeizeActRedeemerFromRelativeInputIdxs
    directoryNodeIdx
    (absoluteToRelativeInputIdxs absoluteInputIdxs)
