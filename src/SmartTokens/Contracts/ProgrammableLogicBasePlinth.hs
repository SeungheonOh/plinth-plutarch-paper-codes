{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module SmartTokens.Contracts.ProgrammableLogicBasePlinth (
  plinthProgrammableLogicBaseScript,
  plinthProgrammableLogicGlobalScript,
) where

import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Foldable (foldr)
import PlutusTx.Prelude

import Plutarch.Script (Script (..))
import PlutusTx.Code (getPlcNoAnn)
import UntypedPlutusCore qualified as UPLC

import SmartTokens.Types.PTokenDirectory (DirectorySetNode (..))
import SmartTokens.Types.ProgrammableLogicGlobal (ProgrammableLogicGlobalRedeemer (..))
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (..))

-- ============================================================================
-- 1. Plinth infrastructure
-- ============================================================================

compiledCodeToScript :: PlutusTx.CompiledCode a -> Script
compiledCodeToScript code =
  let UPLC.Program ann ver body = getPlcNoAnn code
   in Script (UPLC.Program ann ver (UPLC.termMapNames UPLC.unNameDeBruijn body))

{-# INLINEABLE dataEqual #-}
dataEqual :: (PlutusTx.ToData a) => a -> a -> Bool
dataEqual a b = PlutusTx.toBuiltinData a == PlutusTx.toBuiltinData b

-- ============================================================================
-- 2. List operations
-- ============================================================================

{-# INLINEABLE dropList #-}
dropList :: Integer -> [a] -> [a]
dropList n xs
  | n <= 0 = xs
  | otherwise = case xs of
      [] -> []
      (_ : ys) -> dropList (n - 1) ys

{-# INLINEABLE listLength #-}
listLength :: [a] -> Integer
listLength [] = 0
listLength (_ : xs) = 1 + listLength xs

{-# INLINEABLE appendList #-}
appendList :: [a] -> [a] -> [a]
appendList xs ys = foldr (:) ys xs

{-# INLINEABLE pkhElem #-}
pkhElem :: PubKeyHash -> [PubKeyHash] -> Bool
pkhElem _ [] = False
pkhElem pkh (x : xs) = pkh == x || pkhElem pkh xs

{-# INLINEABLE isNullList #-}
isNullList :: [a] -> Bool
isNullList [] = True
isNullList _ = False

{-# INLINEABLE indexList #-}
indexList :: [a] -> Integer -> a
indexList [] _ = traceError "index out of bounds"
indexList (x : xs) n
  | n <= 0 = x
  | otherwise = indexList xs (n - 1)

-- ============================================================================
-- 3. Value helpers
-- ============================================================================

{-# INLINEABLE valueToSortedList #-}
valueToSortedList :: Value -> [(CurrencySymbol, [(TokenName, Integer)])]
valueToSortedList (Value m) = [(cs, Map.toList tns) | (cs, tns) <- Map.toList m]

{-# INLINEABLE sortedListToValue #-}
sortedListToValue :: [(CurrencySymbol, [(TokenName, Integer)])] -> Value
sortedListToValue xs = Value (Map.unsafeFromList [(cs, Map.unsafeFromList tns) | (cs, tns) <- xs])

{-# INLINEABLE emptyValue #-}
emptyValue :: Value
emptyValue = sortedListToValue []

-- | Strip Ada (first entry) from a ledger-provided sorted value.
{-# INLINEABLE stripAda #-}
stripAda :: Value -> Value
stripAda (Value m) =
  case Map.toList m of
    [] -> Value Map.empty
    (_ : xs) -> Value (Map.unsafeFromList xs)

-- | Merge two sorted token-name maps by asset-wise addition.
{-# INLINEABLE tokenPairsUnion #-}
tokenPairsUnion :: [(TokenName, Integer)] -> [(TokenName, Integer)] -> [(TokenName, Integer)]
tokenPairsUnion [] bs = bs
tokenPairsUnion as [] = as
tokenPairsUnion aAll@((tnA, qA) : as) bAll@((tnB, qB) : bs)
  | tnA == tnB = (tnA, qA + qB) : tokenPairsUnion as bs
  | tnA < tnB = (tnA, qA) : tokenPairsUnion as bAll
  | otherwise = (tnB, qB) : tokenPairsUnion aAll bs

-- | Merge two sorted currency-symbol maps by asset-wise addition.
{-# INLINEABLE currencyPairsUnion #-}
currencyPairsUnion
  :: [(CurrencySymbol, [(TokenName, Integer)])]
  -> [(CurrencySymbol, [(TokenName, Integer)])]
  -> [(CurrencySymbol, [(TokenName, Integer)])]
currencyPairsUnion [] bs = bs
currencyPairsUnion as [] = as
currencyPairsUnion aAll@((csA, tnsA) : as) bAll@((csB, tnsB) : bs)
  | csA == csB = (csA, tokenPairsUnion tnsA tnsB) : currencyPairsUnion as bs
  | csA < csB = (csA, tnsA) : currencyPairsUnion as bAll
  | otherwise = (csB, tnsB) : currencyPairsUnion aAll bs

-- | Add two sorted non-Ada Values.
{-# INLINEABLE valueUnion #-}
valueUnion :: Value -> Value -> Value
valueUnion a b = sortedListToValue (currencyPairsUnion (valueToSortedList a) (valueToSortedList b))

-- | Get the quantity of a specific asset in a Value.
{-# INLINEABLE assetQtyInValue #-}
assetQtyInValue :: Value -> CurrencySymbol -> TokenName -> Integer
assetQtyInValue (Value m) cs tn =
  let goCS pairs = case pairs of
        [] -> 0
        ((cs', tns) : rest)
          | cs' == cs -> goTN (Map.toList tns)
          | cs < cs' -> 0
          | otherwise -> goCS rest
      goTN pairs = case pairs of
        [] -> 0
        ((tn', qty) : rest)
          | tn' == tn -> qty
          | tn < tn' -> 0
          | otherwise -> goTN rest
   in goCS (Map.toList m)

-- | Get token pairs for a specific currency symbol from a sorted value.
{-# INLINEABLE tokensForCurrencySymbol #-}
tokensForCurrencySymbol :: CurrencySymbol -> Value -> [(TokenName, Integer)]
tokensForCurrencySymbol targetCs (Value m) =
  let go pairs = case pairs of
        [] -> []
        ((cs, tns) : rest)
          | cs == targetCs -> Map.toList tns
          | targetCs < cs -> []
          | otherwise -> go rest
   in go (Map.toList m)

-- | Negate all quantities in a token-name map.
{-# INLINEABLE negateTokens #-}
negateTokens :: [(TokenName, Integer)] -> [(TokenName, Integer)]
negateTokens [] = []
negateTokens ((tn, qty) : rest) = (tn, negate qty) : negateTokens rest

-- | Subtract token pairs: input - output, skipping zero results.
{-# INLINEABLE subtractTokens #-}
subtractTokens :: [(TokenName, Integer)] -> [(TokenName, Integer)] -> [(TokenName, Integer)]
subtractTokens [] outs = negateTokens outs
subtractTokens ins [] = ins
subtractTokens inAll@((inTn, inQty) : inRest) outAll@((outTn, outQty) : outRest)
  | inTn <= outTn =
      if inTn == outTn
        then
          let diff = inQty - outQty
           in if diff == 0
                then subtractTokens inRest outRest
                else (inTn, diff) : subtractTokens inRest outRest
        else -- outTn > inTn -> token only in input
          (inTn, inQty) : subtractTokens inRest outAll
  -- outTn < inTn -> token only in output
  | otherwise = (outTn, negate outQty) : subtractTokens inAll outRest

-- | Check that actualTokens contain at least requiredTokens quantities.
{-# INLINEABLE tokenPairsContain #-}
tokenPairsContain :: [(TokenName, Integer)] -> [(TokenName, Integer)] -> Bool
tokenPairsContain _ [] = True
tokenPairsContain [] ((_, reqQty) : reqRest) =
  (0 >= reqQty) && tokenPairsContain [] reqRest
tokenPairsContain actAll@((actTn, actQty) : actRest) reqAll@((reqTn, reqQty) : reqRest)
  | actTn == reqTn = (actQty >= reqQty) && tokenPairsContain actRest reqRest
  | actTn < reqTn = tokenPairsContain actRest reqAll
  | otherwise = (0 >= reqQty) && tokenPairsContain actAll reqRest

-- ============================================================================
-- 4. Credential and address helpers
-- ============================================================================

{-# INLINEABLE getPaymentCredential #-}
getPaymentCredential :: Address -> Credential
getPaymentCredential (Address cred _) = cred

{-# INLINEABLE getStakingCredential #-}
getStakingCredential :: Address -> Maybe StakingCredential
getStakingCredential (Address _ mStake) = mStake

-- ============================================================================
-- 5. Withdrawal and script info checking
-- ============================================================================

{-# INLINEABLE isScriptInvokedEntries #-}
isScriptInvokedEntries :: Credential -> [(Credential, Lovelace)] -> Bool
isScriptInvokedEntries _ [] = False
isScriptInvokedEntries cred ((c, _) : rest) =
  c == cred || isScriptInvokedEntries cred rest

{-# INLINEABLE isRewardingScriptInfo #-}
isRewardingScriptInfo :: ScriptInfo -> Bool
isRewardingScriptInfo (RewardingScript _) = True
isRewardingScriptInfo _ = False

{-# INLINEABLE isSpendingPurpose #-}
isSpendingPurpose :: ScriptPurpose -> Bool
isSpendingPurpose (Spending _) = True
isSpendingPurpose _ = False

-- ============================================================================
-- 6. Value CS checking
-- ============================================================================

-- | Check that the first non-Ada policy matches a state-token CS.
{-# INLINEABLE hasCsFirstNonAda #-}
hasCsFirstNonAda :: CurrencySymbol -> Value -> Bool
hasCsFirstNonAda cs (Value m) =
  case Map.toList m of
    (_ : (cs', _) : _) -> cs' == cs
    _ -> False

-- | Safe variant that returns False instead of crashing on missing non-Ada entries.
{-# INLINEABLE hasCsFirstNonAdaOrFalse #-}
hasCsFirstNonAdaOrFalse :: CurrencySymbol -> Value -> Bool
hasCsFirstNonAdaOrFalse = hasCsFirstNonAda

-- ============================================================================
-- 7. Value aggregation
-- ============================================================================

{-# INLINEABLE valueFromCred #-}
valueFromCred
  :: Credential
  -> [PubKeyHash]
  -> [(Credential, Lovelace)]
  -> [TxInInfo]
  -> Value
valueFromCred cred sigs withdrawalEntries inputs =
  go emptyValue inputs
 where
  go acc [] = acc
  go acc (TxInInfo _ txOut : rest) =
    let addr = txOutAddress txOut
        paymentCred = getPaymentCredential addr
     in if paymentCred == cred
          then case getStakingCredential addr of
            Just (StakingHash ownerCred) ->
              case ownerCred of
                PubKeyCredential pkh ->
                  if pkhElem pkh sigs
                    then go (valueUnion acc (stripAda (txOutValue txOut))) rest
                    else traceError "Missing required pk witness"
                ScriptCredential _ ->
                  if isScriptInvokedEntries ownerCred withdrawalEntries
                    then go (valueUnion acc (stripAda (txOutValue txOut))) rest
                    else traceError "Missing required script witness"
            _ -> error ()
          else go acc rest

{-# INLINEABLE valueToCred #-}
valueToCred :: Credential -> [TxOut] -> Value
valueToCred cred outputs =
  go emptyValue outputs
 where
  go acc [] = acc
  go acc (txOut : rest) =
    let paymentCred = getPaymentCredential (txOutAddress txOut)
     in if paymentCred == cred
          then go (valueUnion acc (stripAda (txOutValue txOut))) rest
          else go acc rest

-- ============================================================================
-- 8. Output containment check
-- ============================================================================

{-# INLINEABLE outputsContainExpectedValueAtCred #-}
outputsContainExpectedValueAtCred :: Credential -> [TxOut] -> Value -> Bool
outputsContainExpectedValueAtCred progLogicCred txOutputs expectedValue =
  let expectedCsPairs = valueToSortedList expectedValue
   in case expectedCsPairs of
        [] -> True
        [(expCs, expTnPairs)] ->
          case expTnPairs of
            [(expTn, expQty)] ->
              hasAtLeastAssetInProgOutputs expQty 0 expCs expTn txOutputs
            _ -> multiAssetCheck expectedCsPairs
        _ -> multiAssetCheck expectedCsPairs
 where
  hasAtLeastAssetInProgOutputs reqQty curQty cs tn outs
    | curQty >= reqQty = True
    | otherwise = case outs of
        [] -> curQty >= reqQty
        (txOut : rest) ->
          if getPaymentCredential (txOutAddress txOut) == progLogicCred
            then hasAtLeastAssetInProgOutputs reqQty (curQty + assetQtyInValue (txOutValue txOut) cs tn) cs tn rest
            else hasAtLeastAssetInProgOutputs reqQty curQty cs tn rest

  multiAssetCheck expCsPairs =
    let actualValue = valueToCred progLogicCred txOutputs
     in checkAllExpectedCsPairs actualValue expCsPairs

  checkAllExpectedCsPairs _ [] = True
  checkAllExpectedCsPairs actual ((cs, tnPairs) : rest) =
    checkAllExpectedTnPairs actual cs tnPairs && checkAllExpectedCsPairs actual rest

  checkAllExpectedTnPairs _ _ [] = True
  checkAllExpectedTnPairs actual cs ((tn, qty) : rest) =
    assetQtyInValue actual cs tn >= qty && checkAllExpectedTnPairs actual cs rest

-- ============================================================================
-- 9. Reference input lookup
-- ============================================================================

{-# INLINEABLE findReferenceInputByCS #-}
findReferenceInputByCS :: CurrencySymbol -> [TxInInfo] -> ProgrammableLogicGlobalParams
findReferenceInputByCS _cs [] = traceError "protocol params not found"
findReferenceInputByCS cs (TxInInfo _ txOut : rest) =
  if hasCsFirstNonAdaOrFalse cs (txOutValue txOut)
    then case txOutDatum txOut of
      OutputDatum (Datum d) ->
        case PlutusTx.fromBuiltinData d of
          Just params -> params
          Nothing -> traceError "protocol params datum decode failed"
      _ -> traceError "protocol params datum missing"
    else findReferenceInputByCS cs rest

{-# INLINEABLE decodeDirectoryNode #-}
decodeDirectoryNode :: TxOut -> DirectorySetNode
decodeDirectoryNode txOut =
  case txOutDatum txOut of
    OutputDatum (Datum d) ->
      case PlutusTx.fromBuiltinData d of
        Just node -> node
        Nothing -> traceError "directory node datum decode failed"
    _ -> traceError "directory node datum missing"

-- ============================================================================
-- 10. Transfer logic validation
-- ============================================================================

{-# INLINEABLE checkTransferLogicAndGetProgrammableValue #-}
checkTransferLogicAndGetProgrammableValue
  :: CurrencySymbol
  -> [TxInInfo]
  -> [Integer]
  -> [(Credential, Lovelace)]
  -> Credential
  -> Value
  -> Value
checkTransferLogicAndGetProgrammableValue directoryNodeCS refInputs proofList withdrawalEntries initialCachedTransferScript totalValue =
  let inputCsPairs = valueToSortedList totalValue
   in sortedListToValue (go proofList inputCsPairs [] initialCachedTransferScript)
 where
  go :: [Integer] -> [(CurrencySymbol, [(TokenName, Integer)])] -> [(CurrencySymbol, [(TokenName, Integer)])] -> Credential -> [(CurrencySymbol, [(TokenName, Integer)])]
  go _ [] acc _ = acc
  go [] _ _ _ = traceError "transfer proof missing"
  go (proofIdx : proofsRest) ((currCS, currTokens) : csPairsRest) acc cachedTransferScript =
    let refInput = indexList refInputs proofIdx
        dirNodeTxOut = txInInfoResolved refInput
        dirNode = decodeDirectoryNode dirNodeTxOut
        nodeKey = key dirNode
        nodeNext = next dirNode
        dirNodeTransferLogic = transferLogicScript dirNode
     in if nodeKey < currCS
          then
            if currCS < nodeNext && hasCsFirstNonAda directoryNodeCS (txOutValue dirNodeTxOut)
              then go proofsRest csPairsRest acc cachedTransferScript
              else traceError "dir neg-proof node must cover"
          else
            let transferScriptOk =
                  dirNodeTransferLogic
                    == cachedTransferScript
                    || isScriptInvokedEntries dirNodeTransferLogic withdrawalEntries
                keyMatch = nodeKey == currCS
                validDirNode = hasCsFirstNonAda directoryNodeCS (txOutValue dirNodeTxOut)
             in if transferScriptOk && keyMatch && validDirNode
                  then go proofsRest csPairsRest (appendList acc [(currCS, currTokens)]) dirNodeTransferLogic
                  else
                    if not transferScriptOk
                      then traceError "Missing required transfer script"
                      else
                        if not keyMatch
                          then traceError "directory proof mismatch"
                          else traceError "invalid dir node"

-- ============================================================================
-- 11. Mint logic validation
-- ============================================================================

{-# INLINEABLE checkMintLogicAndGetProgrammableValue #-}
checkMintLogicAndGetProgrammableValue
  :: CurrencySymbol
  -> [TxInInfo]
  -> [Integer]
  -> [(Credential, Lovelace)]
  -> Value
  -> Value
checkMintLogicAndGetProgrammableValue directoryNodeCS refInputs proofList withdrawalEntries totalMintValue =
  let mintedEntries = valueToSortedList totalMintValue
   in sortedListToValue (go proofList mintedEntries [])
 where
  go :: [Integer] -> [(CurrencySymbol, [(TokenName, Integer)])] -> [(CurrencySymbol, [(TokenName, Integer)])] -> [(CurrencySymbol, [(TokenName, Integer)])]
  go proofs [] progMintValue =
    case proofs of
      [] -> progMintValue
      _ -> traceError "extra mint proof"
  go [] (_ : _) _ = traceError "mint proof missing"
  go (proofIdx : proofsRest) ((mintCs, mintTokens) : mintRest) progMintValue =
    let refInput = indexList refInputs proofIdx
        dirNodeTxOut = txInInfoResolved refInput
        dirNode = decodeDirectoryNode dirNodeTxOut
        nodeKey = key dirNode
        nodeNext = next dirNode
        dirNodeTransferLogic = transferLogicScript dirNode
     in if nodeKey == mintCs
          then
            let transferScriptOk = isScriptInvokedEntries dirNodeTransferLogic withdrawalEntries
                validDirNode = hasCsFirstNonAda directoryNodeCS (txOutValue dirNodeTxOut)
             in if transferScriptOk && validDirNode
                  then go proofsRest mintRest (appendList progMintValue [(mintCs, mintTokens)])
                  else
                    if not transferScriptOk
                      then traceError "Missing required transfer script"
                      else traceError "invalid dir node m"
          else
            let coverLower = nodeKey < mintCs
                coverUpper = mintCs < nodeNext
                validDirNode = hasCsFirstNonAda directoryNodeCS (txOutValue dirNodeTxOut)
             in if coverLower && coverUpper && validDirNode
                  then go proofsRest mintRest progMintValue
                  else traceError "dir mint neg-proof node must cover"

-- ============================================================================
-- 12. Redeemer types and offchain helpers
-- ============================================================================
-- (imported from SmartTokens.Contracts.ProgrammableLogicBase)

-- ============================================================================
-- 13. SeizeAct: value delta
-- ============================================================================

{-# INLINEABLE valueEqualsDeltaCurrencySymbol #-}
valueEqualsDeltaCurrencySymbol :: CurrencySymbol -> Value -> Value -> [(TokenName, Integer)]
valueEqualsDeltaCurrencySymbol progCS inputValue outputValue =
  let innerInput = valueToSortedList inputValue
      innerOutput = valueToSortedList outputValue
   in goOuter innerInput innerOutput
 where
  goOuter
    :: [(CurrencySymbol, [(TokenName, Integer)])]
    -> [(CurrencySymbol, [(TokenName, Integer)])]
    -> [(TokenName, Integer)]
  goOuter [] _ = []
  goOuter _ [] = []
  goOuter ((inCS, inTokens) : inRest) ((outCS, outTokens) : outRest)
    | inCS == outCS =
        if inCS == progCS
          then
            if dataEqual
              (sortedListToValue inRest)
              (sortedListToValue outRest)
              then
                subtractTokens inTokens outTokens
              else
                error ()
          else
            if dataEqual
              (Map.unsafeFromList inTokens :: Map.Map TokenName Integer)
              (Map.unsafeFromList outTokens :: Map.Map TokenName Integer)
              then
                goOuter inRest outRest
              else
                error ()
    | dataEqual
        (Map.unsafeFromList inTokens :: Map.Map TokenName Integer)
        (Map.unsafeFromList outTokens :: Map.Map TokenName Integer) =
        []
    | otherwise = error ()

-- ============================================================================
-- 14. SeizeAct: processThirdPartyTransfer
-- ============================================================================

{-# INLINEABLE processThirdPartyTransfer #-}
processThirdPartyTransfer
  :: CurrencySymbol
  -> Credential
  -> [TxInInfo]
  -> [TxOut]
  -> [Integer]
  -> [(TokenName, Integer)]
  -> Bool
processThirdPartyTransfer programmableCS progLogicCred inputs progOutputs inputIdxs mintedTokens =
  go inputIdxs inputs progOutputs []
 where
  go :: [Integer] -> [TxInInfo] -> [TxOut] -> [(TokenName, Integer)] -> Bool
  go [] _remainingInputs remainingOutputs deltaAccumulator =
    let finalDelta = tokenPairsUnion deltaAccumulator mintedTokens
        outputAccumulator = accumulateProgOutputTokens remainingOutputs
     in tokenPairsContain outputAccumulator finalDelta || error ()
  go (relIdx : restIdxs) remainingInputs programmableOutputs deltaAccumulator =
    let dropped = dropList relIdx remainingInputs
     in case dropped of
          [] -> traceError "input index out of bounds"
          (TxInInfo _ programmableInputResolved : afterIdx) ->
            checkCorrespondingPair programmableInputResolved restIdxs afterIdx programmableOutputs deltaAccumulator

  checkCorrespondingPair :: TxOut -> [Integer] -> [TxInInfo] -> [TxOut] -> [(TokenName, Integer)] -> Bool
  checkCorrespondingPair inputTxOut remainingIdxs remainingInputsAfterIdx programmableOutputs deltaAccumulator =
    let inputAddr = txOutAddress inputTxOut
        inputCred = getPaymentCredential inputAddr
     in if inputCred == progLogicCred
          then case programmableOutputs of
            [] -> traceError "no corresponding output"
            (outputTxOut : outputsRest) ->
              let outputAddr = txOutAddress outputTxOut
                  addressMatch = inputAddr == outputAddr
                  datumMatch = txOutDatum inputTxOut == txOutDatum outputTxOut
                  refScriptMatch = txOutReferenceScript inputTxOut == txOutReferenceScript outputTxOut
               in if addressMatch && datumMatch && refScriptMatch
                    then
                      let delta = valueEqualsDeltaCurrencySymbol programmableCS (txOutValue inputTxOut) (txOutValue outputTxOut)
                       in go remainingIdxs remainingInputsAfterIdx outputsRest (tokenPairsUnion delta deltaAccumulator)
                    else
                      if not addressMatch
                        then traceError "corresponding output: address mismatch"
                        else traceError "corresponding output: datum/reference script mismatch"
          else case inputCred of
            ScriptCredential _ -> go remainingIdxs remainingInputsAfterIdx programmableOutputs deltaAccumulator
            _ -> traceError "input index points to pubkey input"

  accumulateProgOutputTokens :: [TxOut] -> [(TokenName, Integer)]
  accumulateProgOutputTokens [] = []
  accumulateProgOutputTokens (txOut : rest) =
    if getPaymentCredential (txOutAddress txOut) == progLogicCred
      then tokenPairsUnion (tokensForCurrencySymbol programmableCS (txOutValue txOut)) (accumulateProgOutputTokens rest)
      else accumulateProgOutputTokens rest

-- ============================================================================
-- 15. Redeemer map helpers
-- ============================================================================

{-# INLINEABLE enforceNSpendRedeemers #-}
enforceNSpendRedeemers :: Integer -> Map.Map ScriptPurpose Redeemer -> Bool
enforceNSpendRedeemers n rdmrs =
  let rdmrsList = Map.toList rdmrs
      dropped = dropList (n - 1) rdmrsList
   in case dropped of
        [] -> False
        ((purpose, _) : rest) ->
          isSpendingPurpose purpose
            && ( case rest of
                   [] -> True
                   ((nextPurpose, _) : _) -> not (isSpendingPurpose nextPurpose)
               )

-- ============================================================================
-- 16. Base validator (mkProgrammableLogicBase)
-- ============================================================================

{-# INLINEABLE mkProgrammableLogicBaseValidator #-}
mkProgrammableLogicBaseValidator :: BuiltinData -> BuiltinData -> ()
mkProgrammableLogicBaseValidator stakeCredData ctxData =
  let stakeCred :: Credential
      stakeCred = PlutusTx.unsafeFromBuiltinData stakeCredData
      ctx :: ScriptContext
      ctx = PlutusTx.unsafeFromBuiltinData ctxData
      txInfo_ = scriptContextTxInfo ctx
      wdrls = Map.toList (txInfoWdrl txInfo_)
   in if hasCred stakeCred wdrls
        then ()
        else traceError "programmable global not invoked"
 where
  hasCred :: Credential -> [(Credential, Lovelace)] -> Bool
  hasCred _ [] = False
  hasCred c ((c', _) : rest) = c == c' || hasCred c rest

-- ============================================================================
-- 17. Global validator (mkProgrammableLogicGlobal)
-- ============================================================================

{-# INLINEABLE mkProgrammableLogicGlobalValidator #-}
mkProgrammableLogicGlobalValidator :: BuiltinData -> BuiltinData -> ()
mkProgrammableLogicGlobalValidator protocolParamsCsData ctxData =
  let protocolParamsCS :: CurrencySymbol
      protocolParamsCS = PlutusTx.unsafeFromBuiltinData protocolParamsCsData
      ctx :: ScriptContext
      ctx = PlutusTx.unsafeFromBuiltinData ctxData
      txInfo_ = scriptContextTxInfo ctx
      scriptInfo_ = scriptContextScriptInfo ctx
      redeemer_ = scriptContextRedeemer ctx
      referenceInputs = txInfoReferenceInputs txInfo_
      withdrawalEntries = Map.toList (txInfoWdrl txInfo_)

      red :: ProgrammableLogicGlobalRedeemer
      red = PlutusTx.unsafeFromBuiltinData (getRedeemer redeemer_)

      ProgrammableLogicGlobalParams{directoryNodeCS, progLogicCred} =
        findReferenceInputByCS protocolParamsCS referenceInputs
   in case red of
        TransferAct{plgrTransferProofs, plgrMintProofs} ->
          let cachedTransferScript0 = case withdrawalEntries of
                ((c, _) : _) -> c
                [] -> traceError "no withdrawals"

              totalProgTokenValue =
                valueFromCred
                  progLogicCred
                  (txInfoSignatories txInfo_)
                  withdrawalEntries
                  (txInfoInputs txInfo_)

              totalProgTokenValue_ =
                checkTransferLogicAndGetProgrammableValue
                  directoryNodeCS
                  referenceInputs
                  plgrTransferProofs
                  withdrawalEntries
                  cachedTransferScript0
                  totalProgTokenValue

              mintValueAsValue = PlutusTx.unsafeFromBuiltinData (PlutusTx.toBuiltinData (txInfoMint txInfo_)) :: Value
              mintCsPairs = valueToSortedList mintValueAsValue

              expectedProgrammableOutputValue =
                if isNullList mintCsPairs
                  then totalProgTokenValue_
                  else
                    let progMintValue =
                          checkMintLogicAndGetProgrammableValue
                            directoryNodeCS
                            referenceInputs
                            plgrMintProofs
                            withdrawalEntries
                            mintValueAsValue
                     in valueUnion totalProgTokenValue_ progMintValue
           in if isRewardingScriptInfo scriptInfo_
                && outputsContainExpectedValueAtCred
                  progLogicCred
                  (txInfoOutputs txInfo_)
                  expectedProgrammableOutputValue
                then ()
                else traceError "prog tokens escape"
        SeizeAct{plgrDirectoryNodeIdx, plgrInputIdxs, plgrOutputsStartIdx, plgrLengthInputIdxs} ->
          let inputIdxsLen = plgrLengthInputIdxs
              remainingOutputs = dropList plgrOutputsStartIdx (txInfoOutputs txInfo_)
              directoryNodeUTxO = indexList referenceInputs plgrDirectoryNodeIdx
              dirNodeTxOut = txInInfoResolved directoryNodeUTxO
              dirNode = decodeDirectoryNode dirNodeTxOut

              programmableCS = key dirNode
              dirNodeIssuerLogic = issuerLogicScript dirNode

              mintValueAsValue = PlutusTx.unsafeFromBuiltinData (PlutusTx.toBuiltinData (txInfoMint txInfo_)) :: Value
              seizeMintedTokens = tokensForCurrencySymbol programmableCS mintValueAsValue

              miniLedgerOk = processThirdPartyTransfer programmableCS progLogicCred (txInfoInputs txInfo_) remainingOutputs plgrInputIdxs seizeMintedTokens
              issuerLogicOk = isScriptInvokedEntries dirNodeIssuerLogic withdrawalEntries
              validDirNode = hasCsFirstNonAda directoryNodeCS (txOutValue dirNodeTxOut)
              spendRedeemersOk = enforceNSpendRedeemers inputIdxsLen (txInfoRedeemers txInfo_)
              inputIdxLenOk = listLength plgrInputIdxs == inputIdxsLen
           in if miniLedgerOk && issuerLogicOk && validDirNode && spendRedeemersOk && inputIdxLenOk
                then ()
                else
                  if not issuerLogicOk
                    then traceError "issuer logic script must be invoked"
                    else
                      if not validDirNode
                        then traceError "directory node is not valid"
                        else
                          if not spendRedeemersOk
                            then traceError "spending redeemer count mismatch"
                            else traceError "input index length mismatch"

-- ============================================================================
-- 18. Compiled scripts
-- ============================================================================

plinthProgrammableLogicBaseScript :: Script
plinthProgrammableLogicBaseScript =
  compiledCodeToScript $$(PlutusTx.compile [||mkProgrammableLogicBaseValidator||])

plinthProgrammableLogicGlobalScript :: Script
plinthProgrammableLogicGlobalScript =
  compiledCodeToScript $$(PlutusTx.compile [||mkProgrammableLogicGlobalValidator||])
