{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Hydra.Contracts.HeadPlinth (
  plinthHeadScript,
) where

import Plutarch.Script (Script (..))
import PlutusLedgerApi.V1.Value (geq, isZero)
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Code (getPlcNoAnn)
import PlutusTx.Foldable qualified as F
import PlutusTx.List qualified as L
import PlutusTx.Prelude
import UntypedPlutusCore qualified as UPLC

import Hydra.Types.HeadState

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

compiledCodeToScript :: PlutusTx.CompiledCode a -> Script
compiledCodeToScript code =
  let UPLC.Program ann ver body = getPlcNoAnn code
   in Script (UPLC.Program ann ver (UPLC.termMapNames UPLC.unNameDeBruijn body))

{-# INLINEABLE listLength #-}
listLength :: [a] -> Integer
listLength [] = 0
listLength (_ : xs) = 1 + listLength xs

{-# INLINEABLE listTake #-}
listTake :: Integer -> [a] -> [a]
listTake n _
  | n <= 0 = []
listTake _ [] = []
listTake n (x : xs) = x : listTake (n - 1) xs

{-# INLINEABLE listDrop #-}
listDrop :: Integer -> [a] -> [a]
listDrop n xs
  | n <= 0 = xs
listDrop _ [] = []
listDrop n (_ : xs) = listDrop (n - 1) xs

{-# INLINEABLE listAll #-}
listAll :: (a -> Bool) -> [a] -> Bool
listAll _ [] = True
listAll p (x : xs) = p x && listAll p xs

{-# INLINEABLE listHead #-}
listHead :: [a] -> a
listHead (x : _) = x
listHead [] = traceError "empty list"

{-# INLINEABLE listTail #-}
listTail :: [a] -> [a]
listTail (_ : xs) = xs
listTail [] = traceError "empty list"

{-# INLINEABLE listNull #-}
listNull :: [a] -> Bool
listNull [] = True
listNull _ = False

{-# INLINEABLE listFind #-}
listFind :: (a -> Bool) -> [a] -> Maybe a
listFind _ [] = Nothing
listFind p (x : xs) = if p x then Just x else listFind p xs

{-# INLINEABLE listElem #-}
listElem :: (Eq a) => a -> [a] -> Bool
listElem _ [] = False
listElem x (y : ys) = x == y || listElem x ys

{-# INLINEABLE listNotElem #-}
listNotElem :: (Eq a) => a -> [a] -> Bool
listNotElem x xs = not (listElem x xs)

{-# INLINEABLE sumIntegers #-}
sumIntegers :: [Integer] -> Integer
sumIntegers xs = F.foldr (+) 0 xs

-- ============================================================================
-- 2. Hydra constants
-- ============================================================================

{-# INLINEABLE hydraHeadV2 #-}
hydraHeadV2 :: BuiltinByteString
hydraHeadV2 = "HydraHeadV2"

-- ============================================================================
-- 3. Utility functions (inlined from Hydra.Contract.Util)
-- ============================================================================

{-# INLINEABLE hasST #-}
hasST :: CurrencySymbol -> Value -> Bool
hasST headPolicyId v =
  case Map.lookup headPolicyId (getValue v) of
    Nothing -> False
    Just tokenMap ->
      case Map.lookup (TokenName hydraHeadV2) tokenMap of
        Nothing -> False
        Just qty -> qty == 1

{-# INLINEABLE mustBurnAllHeadTokens #-}
mustBurnAllHeadTokens :: Value -> CurrencySymbol -> [BuiltinByteString] -> Bool
mustBurnAllHeadTokens mintVal headCurrencySymbol parties =
  traceIfFalse "H14"
    $ burntTokens
    == listLength parties
    + 1
 where
  burntTokens =
    case Map.lookup headCurrencySymbol (getValue mintVal) of
      Nothing -> 0
      Just tokenMap -> negate $ sumIntegers [v | (_, v) <- Map.toList tokenMap]

{-# INLINEABLE mustNotMintOrBurn #-}
mustNotMintOrBurn :: MintValue -> Bool
mustNotMintOrBurn mint =
  traceIfFalse "U01"
    $ Map.null
    $ getValue
    $ unsafeFromBuiltinData (toBuiltinData mint)

{-# INLINEABLE mustPreserveHeadValue #-}
mustPreserveHeadValue :: [TxOut] -> TxOutRef -> [TxInInfo] -> Bool
mustPreserveHeadValue outputs ownRef inputs =
  traceIfFalse "H4"
    $ val'
    `geq` val
 where
  val = case findOwnInput ownRef inputs of
    Nothing -> mempty
    Just (TxInInfo _ txOut) -> txOutValue txOut
  val' = txOutValue $ listHead outputs

{-# INLINEABLE findOwnInput #-}
findOwnInput :: TxOutRef -> [TxInInfo] -> Maybe TxInInfo
findOwnInput ref inputs =
  listFind (\(TxInInfo r _) -> r == ref) inputs

{-# INLINEABLE hashTxOuts #-}
hashTxOuts :: [TxOut] -> BuiltinByteString
hashTxOuts = sha2_256 . F.foldMap (Builtins.serialiseData . toBuiltinData)

{-# INLINEABLE hashPreSerializedCommits #-}
hashPreSerializedCommits :: [Commit] -> BuiltinByteString
hashPreSerializedCommits commits =
  sha2_256
    $ F.foldMap commitPreSerializedOutput
    $ L.sortBy (\a b -> compareRef (commitInput a) (commitInput b)) commits

{-# INLINEABLE compareRef #-}
compareRef :: TxOutRef -> TxOutRef -> Ordering
compareRef (TxOutRef id1 idx1) (TxOutRef id2 idx2) =
  case compare id1 id2 of
    EQ -> compare idx1 idx2
    ord -> ord

{-# INLINEABLE emptyHash #-}
emptyHash :: BuiltinByteString
emptyHash = hashTxOuts []

-- ============================================================================
-- 4. ContestationPeriod helpers
-- ============================================================================

{-# INLINEABLE addContestationPeriod #-}
addContestationPeriod :: POSIXTime -> Integer -> POSIXTime
addContestationPeriod (POSIXTime t) ms = POSIXTime (t + ms)

-- ============================================================================
-- 5. DepositDatum
-- ============================================================================

type DepositDatum = (CurrencySymbol, POSIXTime, [Commit])

{-# INLINEABLE depositDatum #-}
depositDatum :: TxOut -> [Commit]
depositDatum txOut =
  case txOutDatum txOut of
    OutputDatum (Datum d) ->
      case fromBuiltinData @DepositDatum d of
        Just (_, _, commits) -> commits
        Nothing -> []
    _ -> []

-- ============================================================================
-- 6. Head validator helpers
-- ============================================================================

{-# INLINEABLE getHeadInput #-}
getHeadInput :: TxOutRef -> [TxInInfo] -> TxInInfo
getHeadInput ownRef inputs = case findOwnInput ownRef inputs of
  Nothing -> traceError "H8"
  Just x -> x

{-# INLINEABLE getHeadAddress #-}
getHeadAddress :: TxOutRef -> [TxInInfo] -> Address
getHeadAddress ownRef inputs = txOutAddress . txInInfoResolved $ getHeadInput ownRef inputs

{-# INLINEABLE mustNotChangeParameters #-}
mustNotChangeParameters
  :: ([BuiltinByteString], [BuiltinByteString])
  -> (Integer, Integer)
  -> (CurrencySymbol, CurrencySymbol)
  -> Bool
mustNotChangeParameters (parties', parties) (cp', cp) (headId', headId) =
  traceIfFalse "H2"
    $ parties'
    == parties
    && cp'
    == cp
    && headId'
    == headId

{-# INLINEABLE mustBeSignedByParticipant #-}
mustBeSignedByParticipant :: [PubKeyHash] -> [TxInInfo] -> CurrencySymbol -> Bool
mustBeSignedByParticipant signatories inputs headCurrencySymbol =
  case getPubKeyHash <$> signatories of
    [signer] ->
      traceIfFalse "H5"
        $ listElem signer (unTokenName <$> participationTokens)
    [] -> traceError "H6"
    _ -> traceError "H7"
 where
  participationTokens = go inputs
  go [] = []
  go (TxInInfo _ txOut : rest) = findParticipationTokens headCurrencySymbol (txOutValue txOut) L.++ go rest

{-# INLINEABLE findParticipationTokens #-}
findParticipationTokens :: CurrencySymbol -> Value -> [TokenName]
findParticipationTokens headCurrency (Value val) =
  case Map.lookup headCurrency val of
    Nothing -> []
    Just tokenMap ->
      [tn | (tn, n) <- Map.toList tokenMap, n == 1, tn /= TokenName hydraHeadV2]

{-# INLINEABLE headOutputDatum #-}
headOutputDatum :: [TxOut] -> TxOutRef -> [TxInInfo] -> Datum
headOutputDatum outputs ownRef inputs =
  case outputs of
    (o : _)
      | txOutAddress o == getHeadAddress ownRef inputs -> getTxOutDatum o
    _ -> traceError "H11"

{-# INLINEABLE getTxOutDatum #-}
getTxOutDatum :: TxOut -> Datum
getTxOutDatum o =
  case txOutDatum o of
    NoOutputDatum -> traceError "H9"
    OutputDatumHash _ -> traceError "H10"
    OutputDatum d -> d

{-# INLINEABLE makeContestationDeadline #-}
makeContestationDeadline :: Integer -> POSIXTimeRange -> POSIXTime
makeContestationDeadline cperiod validRange =
  case ivTo validRange of
    UpperBound (Finite time) _ -> addContestationPeriod time cperiod
    _ -> traceError "H27"

{-# INLINEABLE decodeHeadOutputClosedDatum #-}
decodeHeadOutputClosedDatum :: [TxOut] -> TxOutRef -> [TxInInfo] -> ClosedDatum
decodeHeadOutputClosedDatum outputs ownRef inputs =
  case fromBuiltinData @State $ getDatum (headOutputDatum outputs ownRef inputs) of
    Just (Closed closedDatum) -> closedDatum
    _ -> traceError "H3"

{-# INLINEABLE decodeHeadOutputOpenDatum #-}
decodeHeadOutputOpenDatum :: [TxOut] -> TxOutRef -> [TxInInfo] -> OpenDatum
decodeHeadOutputOpenDatum outputs ownRef inputs =
  case fromBuiltinData @State $ getDatum (headOutputDatum outputs ownRef inputs) of
    Just (Open openDatum) -> openDatum
    _ -> traceError "H3"

-- ============================================================================
-- 7. Snapshot signature verification
-- ============================================================================

{-# INLINEABLE verifySnapshotSignature #-}
verifySnapshotSignature :: [BuiltinByteString] -> (CurrencySymbol, SnapshotVersion, SnapshotNumber, Hash, Hash, Hash) -> [Signature] -> Bool
verifySnapshotSignature parties msg sigs =
  traceIfFalse "H12"
    $ listLength parties
    == listLength sigs
    && listAll (uncurry $ verifyPartySignature msg) (L.zip parties sigs)

{-# INLINEABLE verifyPartySignature #-}
verifyPartySignature :: (CurrencySymbol, SnapshotVersion, SnapshotNumber, Hash, Hash, Hash) -> BuiltinByteString -> Signature -> Bool
verifyPartySignature (headId, snapshotVersion, snapshotNumber, utxoHash, utxoToCommitHash, utxoToDecommitHash) partyVkey =
  verifyEd25519Signature partyVkey message
 where
  message =
    Builtins.serialiseData (toBuiltinData headId)
      <> Builtins.serialiseData (toBuiltinData snapshotVersion)
      <> Builtins.serialiseData (toBuiltinData snapshotNumber)
      <> Builtins.serialiseData (toBuiltinData utxoHash)
      <> Builtins.serialiseData (toBuiltinData utxoToCommitHash)
      <> Builtins.serialiseData (toBuiltinData utxoToDecommitHash)

-- ============================================================================
-- 8. checkIncrement
-- ============================================================================

{-# INLINEABLE checkIncrement #-}
checkIncrement :: TxOutRef -> TxInfo -> OpenDatum -> IncrementRedeemer -> Bool
checkIncrement ownRef txInfo openBefore redeemer =
  mustNotChangeParameters (prevParties, nextParties) (prevCperiod, nextCperiod) (prevHeadId, nextHeadId)
    && mustIncreaseVersion
    && mustIncreaseValue
    && mustBeSignedByParticipant signatories inputs prevHeadId
    && checkSnapshotSignature
    && claimedDepositIsSpent
 where
  TxInfo{txInfoInputs = inputs, txInfoOutputs = outputs, txInfoSignatories = signatories} = txInfo

  depositInput =
    case listFind (\(TxInInfo ref _) -> ref == incrementRef redeemer) inputs of
      Nothing -> traceError "H44"
      Just i -> i

  commits = depositDatum $ txInInfoResolved depositInput
  depositHash = hashPreSerializedCommits commits
  depositValue = txOutValue $ txInInfoResolved depositInput

  headInValue =
    case listFind (\(TxInInfo _ o) -> hasST prevHeadId (txOutValue o)) inputs of
      Nothing -> traceError "H45"
      Just (TxInInfo _ o) -> txOutValue o

  headOutValue = txOutValue $ listHead outputs

  IncrementRedeemer{incrementSig = sig, incrementSnapshotNumber = sn, incrementRef = _} = redeemer

  claimedDepositIsSpent =
    traceIfFalse "H43"
      $ listElem (incrementRef redeemer) (txInInfoOutRef <$> inputs)

  checkSnapshotSignature =
    verifySnapshotSignature nextParties (nextHeadId, prevVersion, sn, nextUtxoHash, depositHash, emptyHash) sig

  mustIncreaseVersion =
    traceIfFalse "H21"
      $ nextVersion
      == prevVersion
      + 1

  mustIncreaseValue =
    traceIfFalse "H4"
      $ headInValue
      <> depositValue
      == headOutValue

  OpenDatum{openParties = prevParties, openContestationPeriod = prevCperiod, openHeadId = prevHeadId, openVersion = prevVersion} = openBefore
  OpenDatum{openUtxoHash = nextUtxoHash, openParties = nextParties, openContestationPeriod = nextCperiod, openHeadId = nextHeadId, openVersion = nextVersion} = decodeHeadOutputOpenDatum outputs ownRef inputs

-- ============================================================================
-- 9. checkDecrement
-- ============================================================================

{-# INLINEABLE checkDecrement #-}
checkDecrement :: TxOutRef -> TxInfo -> OpenDatum -> DecrementRedeemer -> Bool
checkDecrement ownRef txInfo openBefore redeemer =
  mustNotChangeParameters (prevParties, nextParties) (prevCperiod, nextCperiod) (prevHeadId, nextHeadId)
    && mustIncreaseVersion
    && checkSnapshotSignature
    && mustDecreaseValue
    && mustBeSignedByParticipant signatories inputs prevHeadId
 where
  TxInfo{txInfoInputs = inputs, txInfoOutputs = outputs, txInfoSignatories = signatories} = txInfo

  checkSnapshotSignature =
    verifySnapshotSignature nextParties (nextHeadId, prevVersion, sn, nextUtxoHash, emptyHash, decommitUtxoHash) sig

  mustDecreaseValue =
    traceIfFalse "H4"
      $ headInValue
      == headOutValue
      <> F.foldMap txOutValue decommitOutputs

  mustIncreaseVersion =
    traceIfFalse "H21"
      $ nextVersion
      == prevVersion
      + 1

  decommitUtxoHash = hashTxOuts decommitOutputs

  DecrementRedeemer{decrementSig = sig, decrementSnapshotNumber = sn, decrementNumberOfDecommitOutputs = numDecommit} = redeemer

  OpenDatum{openParties = prevParties, openContestationPeriod = prevCperiod, openHeadId = prevHeadId, openVersion = prevVersion} = openBefore
  OpenDatum{openUtxoHash = nextUtxoHash, openParties = nextParties, openContestationPeriod = nextCperiod, openHeadId = nextHeadId, openVersion = nextVersion} = decodeHeadOutputOpenDatum outputs ownRef inputs

  headOutValue = txOutValue $ listHead outputs
  headInValue = case findOwnInput ownRef inputs of
    Nothing -> mempty
    Just (TxInInfo _ o) -> txOutValue o

  decommitOutputs = listTake numDecommit (listTail outputs)

-- ============================================================================
-- 10. checkClose
-- ============================================================================

{-# INLINEABLE checkClose #-}
checkClose :: TxOutRef -> TxInfo -> OpenDatum -> CloseRedeemer -> Bool
checkClose ownRef txInfo openBefore redeemer =
  mustNotMintOrBurn mint
    && hasBoundedValidity
    && checkDeadline
    && mustBeSignedByParticipant signatories inputs headId
    && mustNotChangeVersion
    && mustBeValidSnapshot
    && mustInitializeContesters
    && mustPreserveHeadValue outputs ownRef inputs
    && mustNotChangeParameters (closedParties closedOut, openParties openBefore) (closedContestationPeriod closedOut, openContestationPeriod openBefore) (closedHeadId closedOut, headId)
 where
  TxInfo{txInfoInputs = inputs, txInfoOutputs = outputs, txInfoValidRange = validRange, txInfoMint = mint, txInfoSignatories = signatories} = txInfo

  OpenDatum{openParties = parties, openUtxoHash = initialUtxoHash, openContestationPeriod = cperiod, openHeadId = headId, openVersion = version} = openBefore

  closedOut = decodeHeadOutputClosedDatum outputs ownRef inputs

  hasBoundedValidity =
    traceIfFalse "H22"
      $ tMax
      - tMin
      <= POSIXTime cperiod

  mustNotChangeVersion =
    traceIfFalse "H13"
      $ closedVersion closedOut
      == version

  mustBeValidSnapshot =
    case redeemer of
      CloseInitial ->
        traceIfFalse "H28"
          $ version
          == 0
          && closedSnapshotNumber closedOut
          == 0
          && closedUtxoHash closedOut
          == initialUtxoHash
      CloseAny{closeAnySig = sig} ->
        traceIfFalse "H46"
          $ closedSnapshotNumber closedOut
          > 0
          && closedAlphaUTxOHash closedOut
          == emptyHash
          && closedOmegaUTxOHash closedOut
          == emptyHash
          && verifySnapshotSignature parties (headId, version, closedSnapshotNumber closedOut, closedUtxoHash closedOut, emptyHash, emptyHash) sig
      CloseUnusedDec{closeUnusedDecSig = sig} ->
        traceIfFalse "H50"
          $ closedAlphaUTxOHash closedOut
          == emptyHash
          && closedOmegaUTxOHash closedOut
          /= emptyHash
          && verifySnapshotSignature parties (headId, version, closedSnapshotNumber closedOut, closedUtxoHash closedOut, emptyHash, closedOmegaUTxOHash closedOut) sig
      CloseUsedDec{closeUsedDecSig = sig, closeUsedDecAlreadyDecommitted = alreadyDecommitted} ->
        traceIfFalse "H51"
          $ closedAlphaUTxOHash closedOut
          == emptyHash
          && closedOmegaUTxOHash closedOut
          == emptyHash
          && verifySnapshotSignature parties (headId, version - 1, closedSnapshotNumber closedOut, closedUtxoHash closedOut, emptyHash, alreadyDecommitted) sig
      CloseUnusedInc{closeUnusedIncSig = sig, closeUnusedIncAlreadyCommitted = alreadyCommitted} ->
        traceIfFalse "H52"
          $ closedAlphaUTxOHash closedOut
          == emptyHash
          && closedOmegaUTxOHash closedOut
          == emptyHash
          && verifySnapshotSignature parties (headId, version, closedSnapshotNumber closedOut, closedUtxoHash closedOut, alreadyCommitted, emptyHash) sig
      CloseUsedInc{closeUsedIncSig = sig, closeUsedIncAlreadyCommitted = alreadyCommitted} ->
        traceIfFalse "H53"
          $ closedAlphaUTxOHash closedOut
          == alreadyCommitted
          && closedOmegaUTxOHash closedOut
          == emptyHash
          && verifySnapshotSignature parties (headId, version - 1, closedSnapshotNumber closedOut, closedUtxoHash closedOut, alreadyCommitted, emptyHash) sig

  checkDeadline =
    traceIfFalse "H23"
      $ closedContestationDeadline closedOut
      == makeContestationDeadline cperiod validRange

  cp = POSIXTime cperiod

  tMax = case ivTo validRange of
    UpperBound (Finite t) _ -> t
    _ -> traceError "H24"

  tMin = case ivFrom validRange of
    LowerBound (Finite t) _ -> t
    _ -> traceError "H25"

  mustInitializeContesters =
    traceIfFalse "H26"
      $ listNull (closedContesters closedOut)

-- ============================================================================
-- 11. checkContest
-- ============================================================================

{-# INLINEABLE checkContest #-}
checkContest :: TxOutRef -> TxInfo -> ClosedDatum -> ContestRedeemer -> Bool
checkContest ownRef txInfo closedDatum redeemer =
  mustNotMintOrBurn mint
    && mustNotChangeVersion
    && mustBeNewer
    && mustBeValidSnapshot
    && mustBeSignedByParticipant signatories inputs headId
    && checkSignedParticipantContestOnlyOnce
    && mustBeWithinContestationPeriod
    && mustUpdateContesters
    && mustPushDeadline
    && mustNotChangeParameters (closedParties closedOut, closedParties closedDatum) (closedContestationPeriod closedOut, closedContestationPeriod closedDatum) (closedHeadId closedOut, headId)
    && mustPreserveHeadValue outputs ownRef inputs
 where
  TxInfo{txInfoInputs = inputs, txInfoOutputs = outputs, txInfoValidRange = validRange, txInfoMint = mint, txInfoSignatories = signatories} = txInfo
  closedOut = decodeHeadOutputClosedDatum outputs ownRef inputs
  headId = closedHeadId closedDatum
  parties = closedParties closedDatum
  version = closedVersion closedDatum
  contestationDeadline = closedContestationDeadline closedDatum
  contestationPeriod = closedContestationPeriod closedDatum
  snapshotNum = closedSnapshotNumber closedDatum
  contesters = closedContesters closedDatum

  mustBeNewer =
    traceIfFalse "H29"
      $ closedSnapshotNumber closedOut
      > snapshotNum

  mustNotChangeVersion =
    traceIfFalse "H13"
      $ closedVersion closedOut
      == version

  mustBeValidSnapshot =
    case redeemer of
      ContestCurrent{contestCurrentSig = sig} ->
        traceIfFalse "H37"
          $ closedAlphaUTxOHash closedOut
          == emptyHash
          && closedOmegaUTxOHash closedOut
          == emptyHash
          && verifySnapshotSignature parties (headId, version, closedSnapshotNumber closedOut, closedUtxoHash closedOut, emptyHash, emptyHash) sig
      ContestUsedDec{contestUsedDecSig = sig, contestUsedDecAlreadyDecommitted = alreadyDecommitted} ->
        traceIfFalse "H38"
          $ closedAlphaUTxOHash closedOut
          == emptyHash
          && closedOmegaUTxOHash closedOut
          == emptyHash
          && verifySnapshotSignature parties (headId, version - 1, closedSnapshotNumber closedOut, closedUtxoHash closedOut, emptyHash, alreadyDecommitted) sig
      ContestUnusedDec{contestUnusedDecSig = sig} ->
        traceIfFalse "H47"
          $ closedAlphaUTxOHash closedOut
          == emptyHash
          && verifySnapshotSignature parties (headId, version, closedSnapshotNumber closedOut, closedUtxoHash closedOut, emptyHash, closedOmegaUTxOHash closedOut) sig
      ContestUnusedInc{contestUnusedIncSig = sig, contestUnusedIncAlreadyCommitted = alreadyCommitted} ->
        traceIfFalse "H48"
          $ closedOmegaUTxOHash closedOut
          == emptyHash
          && verifySnapshotSignature parties (headId, version - 1, closedSnapshotNumber closedOut, closedUtxoHash closedOut, alreadyCommitted, emptyHash) sig
      ContestUsedInc{contestUsedIncSig = sig} ->
        traceIfFalse "H49"
          $ closedOmegaUTxOHash closedOut
          == emptyHash
          && verifySnapshotSignature parties (headId, version, closedSnapshotNumber closedOut, closedUtxoHash closedOut, closedAlphaUTxOHash closedOut, emptyHash) sig

  mustBeWithinContestationPeriod =
    case ivTo validRange of
      UpperBound (Finite time) _ ->
        traceIfFalse "H30"
          $ time
          <= contestationDeadline
      _ -> traceError "H31"

  mustPushDeadline =
    if listLength (closedContesters closedOut) == listLength parties
      then
        traceIfFalse "H32"
          $ closedContestationDeadline closedOut
          == contestationDeadline
      else
        traceIfFalse "H33"
          $ closedContestationDeadline closedOut
          == addContestationPeriod contestationDeadline contestationPeriod

  mustUpdateContesters =
    traceIfFalse "H34"
      $ closedContesters closedOut
      == contester
      : contesters

  contester =
    case signatories of
      [signer] -> signer
      _ -> traceError "H35"

  checkSignedParticipantContestOnlyOnce =
    traceIfFalse "H36"
      $ listNotElem contester contesters

-- ============================================================================
-- 12. headIsFinalizedWith (Fanout)
-- ============================================================================

{-# INLINEABLE headIsFinalizedWith #-}
headIsFinalizedWith :: TxInfo -> ClosedDatum -> Integer -> Integer -> Integer -> Bool
headIsFinalizedWith txInfo closedDatum numberOfFanoutOutputs numberOfCommitOutputs numberOfDecommitOutputs =
  mustBurnAllHeadTokens mintedVal headId parties
    && hasSameUTxOHash
    && hasSameCommitUTxOHash
    && hasSameDecommitUTxOHash
    && afterContestationDeadline
 where
  TxInfo{txInfoMint = mint, txInfoOutputs = outputs, txInfoValidRange = validRange} = txInfo
  mintedVal = unsafeFromBuiltinData (toBuiltinData mint)
  headId = closedHeadId closedDatum
  parties = closedParties closedDatum
  utxoHash = closedUtxoHash closedDatum
  alphaUTxOHash = closedAlphaUTxOHash closedDatum
  omegaUTxOHash = closedOmegaUTxOHash closedDatum
  contestationDeadline = closedContestationDeadline closedDatum

  hasSameUTxOHash =
    traceIfFalse "H39"
      $ hashTxOuts (listTake numberOfFanoutOutputs outputs)
      == utxoHash

  hasSameCommitUTxOHash =
    traceIfFalse "H54"
      $ alphaUTxOHash
      == hashTxOuts (listTake numberOfCommitOutputs (listDrop numberOfFanoutOutputs outputs))

  hasSameDecommitUTxOHash =
    traceIfFalse "H40"
      $ omegaUTxOHash
      == hashTxOuts (listTake numberOfDecommitOutputs (listDrop numberOfFanoutOutputs outputs))

  afterContestationDeadline =
    case ivFrom validRange of
      LowerBound (Finite time) _ ->
        traceIfFalse "H41"
          $ time
          > contestationDeadline
      _ -> traceError "H42"

-- ============================================================================
-- 13. Main validator
-- ============================================================================

{-# INLINEABLE headValidator #-}
headValidator :: State -> Input -> TxOutRef -> TxInfo -> Bool
headValidator oldState input ownRef txInfo =
  case (oldState, input) of
    (Open openDatum, Increment red) -> checkIncrement ownRef txInfo openDatum red
    (Open openDatum, Decrement red) -> checkDecrement ownRef txInfo openDatum red
    (Open openDatum, Close red) -> checkClose ownRef txInfo openDatum red
    (Closed closedDatum, Contest red) -> checkContest ownRef txInfo closedDatum red
    (Closed closedDatum, Fanout{fanoutNumberOfFanoutOutputs, fanoutNumberOfCommitOutputs, fanoutNumberOfDecommitOutputs}) ->
      headIsFinalizedWith txInfo closedDatum fanoutNumberOfFanoutOutputs fanoutNumberOfCommitOutputs fanoutNumberOfDecommitOutputs
    _ -> traceError "H1"

-- ============================================================================
-- 14. Entry point and compiled script
-- ============================================================================

{-# INLINEABLE mkHeadValidator #-}
mkHeadValidator :: BuiltinData -> ()
mkHeadValidator ctxData =
  let ctx = unsafeFromBuiltinData @ScriptContext ctxData
      ScriptContext{scriptContextTxInfo = txInfo, scriptContextRedeemer = redeemer, scriptContextScriptInfo = scriptInfo} = ctx
   in case scriptInfo of
        SpendingScript ownRef (Just (Datum datumData)) ->
          let state = unsafeFromBuiltinData @State datumData
              input = unsafeFromBuiltinData @Input $ getRedeemer redeemer
           in if headValidator state input ownRef txInfo then () else error ()
        _ -> error ()

plinthHeadScript :: Script
plinthHeadScript =
  compiledCodeToScript $$(PlutusTx.compile [||mkHeadValidator||])
