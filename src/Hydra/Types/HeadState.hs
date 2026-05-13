{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Hydra.Types.HeadState where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift ()
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PPubKeyHash, PTxOutRef)
import Plutarch.Prelude
import PlutusLedgerApi.Data.V3 qualified as DV3
import PlutusLedgerApi.V3 (
  BuiltinByteString,
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
  TxOutRef,
 )
import PlutusTx qualified
import PlutusTx.AsData (asData)
import PlutusTx.Builtins (BuiltinData)

type SnapshotNumber = Integer

type SnapshotVersion = Integer

type Hash = BuiltinByteString

type Signature = BuiltinByteString

-- ============================================================================
-- Plinth: Commit
-- ============================================================================

data Commit = Commit
  { commitInput :: TxOutRef
  , commitPreSerializedOutput :: BuiltinByteString
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''Commit [('Commit, 0)]

-- ============================================================================
-- Plinth: OpenDatum
-- ============================================================================

data OpenDatum = OpenDatum
  { openHeadSeed :: TxOutRef
  , openHeadId :: CurrencySymbol
  , openParties :: [BuiltinByteString]
  , openContestationPeriod :: Integer
  , openVersion :: SnapshotVersion
  , openUtxoHash :: Hash
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''OpenDatum [('OpenDatum, 0)]

-- ============================================================================
-- Plinth: ClosedDatum
-- ============================================================================

data ClosedDatum = ClosedDatum
  { closedHeadId :: CurrencySymbol
  , closedParties :: [BuiltinByteString]
  , closedContestationPeriod :: Integer
  , closedVersion :: SnapshotVersion
  , closedSnapshotNumber :: SnapshotNumber
  , closedUtxoHash :: Hash
  , closedAlphaUTxOHash :: Hash
  , closedOmegaUTxOHash :: Hash
  , closedContesters :: [PubKeyHash]
  , closedContestationDeadline :: POSIXTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''ClosedDatum [('ClosedDatum, 0)]

-- ============================================================================
-- Plinth: State (datum)
-- ============================================================================

data State
  = Open OpenDatum
  | Closed ClosedDatum
  | Final
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''State [('Open, 0), ('Closed, 1), ('Final, 2)]

-- ============================================================================
-- Plinth: CloseRedeemer
-- ============================================================================

data CloseRedeemer
  = CloseInitial
  | CloseAny
      { closeAnySig :: [Signature]
      }
  | CloseUnusedDec
      { closeUnusedDecSig :: [Signature]
      }
  | CloseUsedDec
      { closeUsedDecSig :: [Signature]
      , closeUsedDecAlreadyDecommitted :: Hash
      }
  | CloseUnusedInc
      { closeUnusedIncSig :: [Signature]
      , closeUnusedIncAlreadyCommitted :: Hash
      }
  | CloseUsedInc
      { closeUsedIncSig :: [Signature]
      , closeUsedIncAlreadyCommitted :: Hash
      }
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''CloseRedeemer
  [ ('CloseInitial, 0)
  , ('CloseAny, 1)
  , ('CloseUnusedDec, 2)
  , ('CloseUsedDec, 3)
  , ('CloseUnusedInc, 4)
  , ('CloseUsedInc, 5)
  ]

-- ============================================================================
-- Plinth: ContestRedeemer
-- ============================================================================

data ContestRedeemer
  = ContestCurrent
      { contestCurrentSig :: [Signature]
      }
  | ContestUsedDec
      { contestUsedDecSig :: [Signature]
      , contestUsedDecAlreadyDecommitted :: Hash
      }
  | ContestUnusedDec
      { contestUnusedDecSig :: [Signature]
      }
  | ContestUnusedInc
      { contestUnusedIncSig :: [Signature]
      , contestUnusedIncAlreadyCommitted :: Hash
      }
  | ContestUsedInc
      { contestUsedIncSig :: [Signature]
      }
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''ContestRedeemer
  [ ('ContestCurrent, 0)
  , ('ContestUsedDec, 1)
  , ('ContestUnusedDec, 2)
  , ('ContestUnusedInc, 3)
  , ('ContestUsedInc, 4)
  ]

-- ============================================================================
-- Plinth: IncrementRedeemer
-- ============================================================================

data IncrementRedeemer = IncrementRedeemer
  { incrementSig :: [Signature]
  , incrementSnapshotNumber :: SnapshotNumber
  , incrementRef :: TxOutRef
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''IncrementRedeemer [('IncrementRedeemer, 0)]

-- ============================================================================
-- Plinth: DecrementRedeemer
-- ============================================================================

data DecrementRedeemer = DecrementRedeemer
  { decrementSig :: [Signature]
  , decrementSnapshotNumber :: SnapshotNumber
  , decrementNumberOfDecommitOutputs :: Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''DecrementRedeemer [('DecrementRedeemer, 0)]

-- ============================================================================
-- Plinth: Input (redeemer)
-- ============================================================================

data Input
  = CollectCom
  | Increment IncrementRedeemer
  | Decrement DecrementRedeemer
  | Close CloseRedeemer
  | Contest ContestRedeemer
  | Abort
  | Fanout
      { fanoutNumberOfFanoutOutputs :: Integer
      , fanoutNumberOfCommitOutputs :: Integer
      , fanoutNumberOfDecommitOutputs :: Integer
      }
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''Input
  [ ('CollectCom, 0)
  , ('Increment, 1)
  , ('Decrement, 2)
  , ('Close, 3)
  , ('Contest, 4)
  , ('Abort, 5)
  , ('Fanout, 6)
  ]

-- ============================================================================
-- Plinth (AsData): OpenDatumD
-- ============================================================================

$( asData
     [d|
       data OpenDatumD = OpenDatumD
         { openHeadSeedD :: DV3.TxOutRef
         , openHeadIdD :: DV3.CurrencySymbol
         , openPartiesD :: [BuiltinByteString]
         , openContestationPeriodD :: Integer
         , openVersionD :: Integer
         , openUtxoHashD :: BuiltinByteString
         }
         deriving newtype (PlutusTx.UnsafeFromData, PlutusTx.ToData)
       |]
 )

-- ============================================================================
-- Plinth (AsData): ClosedDatumD
-- ============================================================================

$( asData
     [d|
       data ClosedDatumD = ClosedDatumD
         { closedHeadIdD :: DV3.CurrencySymbol
         , closedPartiesD :: [BuiltinByteString]
         , closedContestationPeriodD :: Integer
         , closedVersionD :: Integer
         , closedSnapshotNumberD :: Integer
         , closedUtxoHashD :: BuiltinByteString
         , closedAlphaUTxOHashD :: BuiltinByteString
         , closedOmegaUTxOHashD :: BuiltinByteString
         , closedContestersD :: [DV3.PubKeyHash]
         , closedContestationDeadlineD :: Integer
         }
         deriving newtype (PlutusTx.UnsafeFromData, PlutusTx.ToData)
       |]
 )

-- ============================================================================
-- Plinth (AsData): StateD
-- ============================================================================

$( asData
     [d|
       data StateD
         = OpenD BuiltinData
         | ClosedD BuiltinData
         | FinalD
         deriving newtype (PlutusTx.UnsafeFromData)
       |]
 )

-- ============================================================================
-- Plinth (AsData): CloseRedeemerD
-- ============================================================================

$( asData
     [d|
       data CloseRedeemerD
         = CloseInitialD
         | CloseAnyD [BuiltinByteString]
         | CloseUnusedDecD [BuiltinByteString]
         | CloseUsedDecD [BuiltinByteString] BuiltinByteString
         | CloseUnusedIncD [BuiltinByteString] BuiltinByteString
         | CloseUsedIncD [BuiltinByteString] BuiltinByteString
         deriving newtype (PlutusTx.UnsafeFromData)
       |]
 )

-- ============================================================================
-- Plinth (AsData): ContestRedeemerD
-- ============================================================================

$( asData
     [d|
       data ContestRedeemerD
         = ContestCurrentD [BuiltinByteString]
         | ContestUsedDecD [BuiltinByteString] BuiltinByteString
         | ContestUnusedDecD [BuiltinByteString]
         | ContestUnusedIncD [BuiltinByteString] BuiltinByteString
         | ContestUsedIncD [BuiltinByteString]
         deriving newtype (PlutusTx.UnsafeFromData)
       |]
 )

-- ============================================================================
-- Plinth (AsData): IncrementRedeemerD
-- ============================================================================

$( asData
     [d|
       data IncrementRedeemerD = IncrementRedeemerD
         { incrementSigD :: [BuiltinByteString]
         , incrementSnapshotNumberD :: Integer
         , incrementRefD :: DV3.TxOutRef
         }
         deriving newtype (PlutusTx.UnsafeFromData)
       |]
 )

-- ============================================================================
-- Plinth (AsData): DecrementRedeemerD
-- ============================================================================

$( asData
     [d|
       data DecrementRedeemerD = DecrementRedeemerD
         { decrementSigD :: [BuiltinByteString]
         , decrementSnapshotNumberD :: Integer
         , decrementNumberOfDecommitOutputsD :: Integer
         }
         deriving newtype (PlutusTx.UnsafeFromData)
       |]
 )

-- ============================================================================
-- Plinth (AsData): InputD
-- ============================================================================

$( asData
     [d|
       data InputD
         = CollectComD
         | IncrementD BuiltinData
         | DecrementD BuiltinData
         | CloseD BuiltinData
         | ContestD BuiltinData
         | AbortD
         | FanoutD Integer Integer Integer
         deriving newtype (PlutusTx.UnsafeFromData)
       |]
 )

-- ============================================================================
-- Plutarch: PCommit
-- ============================================================================

data PCommit (s :: S) = PCommit
  { pcommitInput :: Term s (PAsData PTxOutRef)
  , pcommitPreSerializedOutput :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCommit)

deriving via
  DeriveDataPLiftable PCommit Commit
  instance
    PLiftable PCommit

-- ============================================================================
-- Plutarch: POpenDatum
-- ============================================================================

data POpenDatum (s :: S) = POpenDatum
  { popenHeadSeed :: Term s (PAsData PTxOutRef)
  , popenHeadId :: Term s (PAsData PCurrencySymbol)
  , popenParties :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , popenContestationPeriod :: Term s (PAsData PInteger)
  , popenVersion :: Term s (PAsData PInteger)
  , popenUtxoHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POpenDatum)

deriving via
  DeriveDataPLiftable POpenDatum OpenDatum
  instance
    PLiftable POpenDatum

-- ============================================================================
-- Plutarch: PClosedDatum
-- ============================================================================

data PClosedDatum (s :: S) = PClosedDatum
  { pclosedHeadId :: Term s (PAsData PCurrencySymbol)
  , pclosedParties :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pclosedContestationPeriod :: Term s (PAsData PInteger)
  , pclosedVersion :: Term s (PAsData PInteger)
  , pclosedSnapshotNumber :: Term s (PAsData PInteger)
  , pclosedUtxoHash :: Term s (PAsData PByteString)
  , pclosedAlphaUTxOHash :: Term s (PAsData PByteString)
  , pclosedOmegaUTxOHash :: Term s (PAsData PByteString)
  , pclosedContesters :: Term s (PAsData (PBuiltinList (PAsData PPubKeyHash)))
  , pclosedContestationDeadline :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PClosedDatum)

deriving via
  DeriveDataPLiftable PClosedDatum ClosedDatum
  instance
    PLiftable PClosedDatum

-- ============================================================================
-- Plutarch: PState
-- ============================================================================

data PState (s :: S)
  = POpen {pstateOpen :: Term s (PAsData POpenDatum)}
  | PClosed {pstateClosed :: Term s (PAsData PClosedDatum)}
  | PFinal
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PState)

deriving via
  DeriveDataPLiftable PState State
  instance
    PLiftable PState

-- ============================================================================
-- Plutarch: PCloseRedeemer
-- ============================================================================

data PCloseRedeemer (s :: S)
  = PCloseInitial
  | PCloseAny
      { pcloseAnySig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      }
  | PCloseUnusedDec
      { pcloseUnusedDecSig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      }
  | PCloseUsedDec
      { pcloseUsedDecSig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      , pcloseUsedDecAlreadyDecommitted :: Term s (PAsData PByteString)
      }
  | PCloseUnusedInc
      { pcloseUnusedIncSig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      , pcloseUnusedIncAlreadyCommitted :: Term s (PAsData PByteString)
      }
  | PCloseUsedInc
      { pcloseUsedIncSig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      , pcloseUsedIncAlreadyCommitted :: Term s (PAsData PByteString)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCloseRedeemer)

deriving via
  DeriveDataPLiftable PCloseRedeemer CloseRedeemer
  instance
    PLiftable PCloseRedeemer

-- ============================================================================
-- Plutarch: PContestRedeemer
-- ============================================================================

data PContestRedeemer (s :: S)
  = PContestCurrent
      { pcontestCurrentSig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      }
  | PContestUsedDec
      { pcontestUsedDecSig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      , pcontestUsedDecAlreadyDecommitted :: Term s (PAsData PByteString)
      }
  | PContestUnusedDec
      { pcontestUnusedDecSig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      }
  | PContestUnusedInc
      { pcontestUnusedIncSig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      , pcontestUnusedIncAlreadyCommitted :: Term s (PAsData PByteString)
      }
  | PContestUsedInc
      { pcontestUsedIncSig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PContestRedeemer)

deriving via
  DeriveDataPLiftable PContestRedeemer ContestRedeemer
  instance
    PLiftable PContestRedeemer

-- ============================================================================
-- Plutarch: PIncrementRedeemer
-- ============================================================================

data PIncrementRedeemer (s :: S) = PIncrementRedeemer
  { pincrementSig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pincrementSnapshotNumber :: Term s (PAsData PInteger)
  , pincrementRef :: Term s (PAsData PTxOutRef)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PIncrementRedeemer)

deriving via
  DeriveDataPLiftable PIncrementRedeemer IncrementRedeemer
  instance
    PLiftable PIncrementRedeemer

-- ============================================================================
-- Plutarch: PDecrementRedeemer
-- ============================================================================

data PDecrementRedeemer (s :: S) = PDecrementRedeemer
  { pdecrementSig :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pdecrementSnapshotNumber :: Term s (PAsData PInteger)
  , pdecrementNumberOfDecommitOutputs :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDecrementRedeemer)

deriving via
  DeriveDataPLiftable PDecrementRedeemer DecrementRedeemer
  instance
    PLiftable PDecrementRedeemer

-- ============================================================================
-- Plutarch: PInput
-- ============================================================================

data PInput (s :: S)
  = PCollectCom
  | PIncrement {pinput'increment :: Term s (PAsData PIncrementRedeemer)}
  | PDecrement {pinput'decrement :: Term s (PAsData PDecrementRedeemer)}
  | PClose {pinput'close :: Term s (PAsData PCloseRedeemer)}
  | PContest {pinput'contest :: Term s (PAsData PContestRedeemer)}
  | PAbort
  | PFanout
      { pfanoutNumberOfFanoutOutputs :: Term s (PAsData PInteger)
      , pfanoutNumberOfCommitOutputs :: Term s (PAsData PInteger)
      , pfanoutNumberOfDecommitOutputs :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PInput)

deriving via
  DeriveDataPLiftable PInput Input
  instance
    PLiftable PInput
