{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists -Wno-missing-export-lists -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fplugin Plinth.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:no-preserve-logging #-}

module SmartTokens.Contracts.ProgrammableLogicBasePlinth (
  plinthProgrammableLogicBaseScript,
  plinthProgrammableLogicGlobalScript,
) where

import Plinth.Plugin
import Plutarch.Script (Script (..))
import PlutusLedgerApi.Data.V3
import PlutusLedgerApi.V1.Data.Value
import PlutusLedgerApi.V2.Data.Tx (matchOutputDatum)
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Code (getPlcNoAnn)
import PlutusTx.Data.AssocMap qualified as DMap
import PlutusTx.Data.List qualified as DList
import PlutusTx.Prelude
import UntypedPlutusCore qualified as UPLC

import SmartTokens.Types.PTokenDirectory (
  DirectorySetNodeD,
  issuerLogicScriptD,
  keyD,
  nextD,
  transferLogicScriptD,
  pattern DirectorySetNodeD,
 )
import SmartTokens.Types.ProgrammableLogicGlobal (
  ProgrammableLogicGlobalRedeemer,
  plgrDirectoryNodeIdx,
  plgrInputIdxs,
  plgrLengthInputIdxs,
  plgrMintProofs,
  plgrOutputsStartIdx,
  plgrTransferProofs,
  pattern SeizeAct,
  pattern TransferAct,
 )
import SmartTokens.Types.ProtocolParams (
  ProgrammableLogicGlobalParamsD,
  directoryNodeCSD,
  progLogicCredD,
  pattern ProgrammableLogicGlobalParamsD,
 )

type BIPairs = BI.BuiltinList (BI.BuiltinPair BI.BuiltinData BI.BuiltinData)
type BIList = BI.BuiltinList BI.BuiltinData

compiledCodeToScript :: PlutusTx.CompiledCode a -> Script
compiledCodeToScript code =
  let UPLC.Program ann ver body = getPlcNoAnn code
   in Script (UPLC.Program ann ver (UPLC.termMapNames UPLC.unNameDeBruijn body))

{-# INLINEABLE dataEqual #-}
dataEqual :: (PlutusTx.ToData a) => a -> a -> Bool
dataEqual a b = PlutusTx.toBuiltinData a == PlutusTx.toBuiltinData b

{-# INLINEABLE nilPairs #-}
nilPairs :: BIPairs
nilPairs = BI.mkNilPairData BI.unitval

{-# INLINEABLE consPair #-}
consPair :: BI.BuiltinData -> BI.BuiltinData -> BIPairs -> BIPairs
consPair k v = BI.mkCons (BI.mkPairData k v)

{-# INLINEABLE dlistToBI #-}
dlistToBI :: DList.List a -> BIList
dlistToBI = DList.toBuiltinList

{-# INLINEABLE biDrop #-}
biDrop :: Integer -> BIList -> BIList
biDrop n xs
  | n <= 0 = xs
  | otherwise = Builtins.matchList' xs xs (\_ ys -> biDrop (n - 1) ys)

{-# INLINEABLE biIndex #-}
biIndex :: BIList -> Integer -> BI.BuiltinData
biIndex xs n =
  BI.unsafeCaseList
    (\h t -> if n <= 0 then h else biIndex t (n - 1))
    xs

{-# INLINEABLE biReversePairs #-}
biReversePairs :: BIPairs -> BIPairs
biReversePairs = go nilPairs
 where
  go acc xs =
    Builtins.matchList' xs acc (\h t -> go (BI.mkCons h acc) t)

{-# INLINEABLE pkhElem #-}
pkhElem :: PubKeyHash -> DList.List PubKeyHash -> Bool
pkhElem = DList.elem

{-# INLINEABLE stripAdaBI #-}
stripAdaBI :: BI.BuiltinData -> BIPairs
stripAdaBI valData =
  let bl = BI.unsafeDataAsMap valData
   in Builtins.matchList' bl nilPairs (\_ rest -> rest)

{-# INLINEABLE biTokenUnion #-}
biTokenUnion :: BIPairs -> BIPairs -> BIPairs
biTokenUnion xs ys =
  Builtins.matchList' xs ys $ \xh xr ->
    Builtins.matchList' ys xs $ \yh yr ->
      let xk = BI.fst xh
          yk = BI.fst yh
          xb = BI.unsafeDataAsB xk
          yb = BI.unsafeDataAsB yk
       in if Builtins.equalsByteString xb yb
            then consPair xk (BI.mkI (BI.unsafeDataAsI (BI.snd xh) + BI.unsafeDataAsI (BI.snd yh))) (biTokenUnion xr yr)
            else
              if Builtins.lessThanByteString xb yb
                then BI.mkCons xh (biTokenUnion xr ys)
                else BI.mkCons yh (biTokenUnion xs yr)

{-# INLINEABLE biCurrencyUnion #-}
biCurrencyUnion :: BIPairs -> BIPairs -> BIPairs
biCurrencyUnion xs ys =
  Builtins.matchList' xs ys $ \xh xr ->
    Builtins.matchList' ys xs $ \yh yr ->
      let xk = BI.fst xh
          yk = BI.fst yh
          xb = BI.unsafeDataAsB xk
          yb = BI.unsafeDataAsB yk
       in if Builtins.equalsByteString xb yb
            then consPair xk (BI.mkMap (biTokenUnion (BI.unsafeDataAsMap (BI.snd xh)) (BI.unsafeDataAsMap (BI.snd yh)))) (biCurrencyUnion xr yr)
            else
              if Builtins.lessThanByteString xb yb
                then BI.mkCons xh (biCurrencyUnion xr ys)
                else BI.mkCons yh (biCurrencyUnion xs yr)

{-# INLINEABLE valueUnion #-}
valueUnion :: Value -> Value -> Value
valueUnion a b = Value (DMap.unsafeFromBuiltinList (biCurrencyUnion (DMap.toBuiltinList (getValue a)) (DMap.toBuiltinList (getValue b))))

{-# INLINEABLE biNegateTokens #-}
biNegateTokens :: BIPairs -> BIPairs
biNegateTokens xs =
  Builtins.matchList' xs nilPairs $ \h t ->
    consPair (BI.fst h) (BI.mkI (negate (BI.unsafeDataAsI (BI.snd h)))) (biNegateTokens t)

{-# INLINEABLE biSubtractTokens #-}
biSubtractTokens :: BIPairs -> BIPairs -> BIPairs
biSubtractTokens ins outs =
  Builtins.matchList
    ins
    (const (biNegateTokens outs))
    ( \inH inR ->
        Builtins.matchList'
          outs
          ins
          ( \outH outR ->
              let inK = BI.fst inH
                  outK = BI.fst outH
                  inB = BI.unsafeDataAsB inK
                  outB = BI.unsafeDataAsB outK
               in if Builtins.lessThanByteString inB outB
                    then BI.mkCons inH (biSubtractTokens inR outs)
                    else
                      if Builtins.equalsByteString inB outB
                        then
                          let diff = BI.unsafeDataAsI (BI.snd inH) - BI.unsafeDataAsI (BI.snd outH)
                           in if diff == 0
                                then biSubtractTokens inR outR
                                else consPair inK (BI.mkI diff) (biSubtractTokens inR outR)
                        else consPair outK (BI.mkI (negate (BI.unsafeDataAsI (BI.snd outH)))) (biSubtractTokens ins outR)
          )
    )

{-# INLINEABLE biTokenPairsContain #-}
biTokenPairsContain :: BIPairs -> BIPairs -> Bool
biTokenPairsContain act req =
  Builtins.matchList'
    req
    True
    ( \reqH reqR ->
        let reqQty = BI.unsafeDataAsI (BI.snd reqH)
         in Builtins.matchList
              act
              (const ((0 >= reqQty) && biTokenPairsContain nilPairs reqR))
              ( \actH actR ->
                  let actK = BI.unsafeDataAsB (BI.fst actH); reqK = BI.unsafeDataAsB (BI.fst reqH)
                   in if Builtins.equalsByteString actK reqK
                        then (BI.unsafeDataAsI (BI.snd actH) >= reqQty) && biTokenPairsContain actR reqR
                        else
                          if Builtins.lessThanByteString actK reqK
                            then biTokenPairsContain actR req
                            else (0 >= reqQty) && biTokenPairsContain act reqR
              )
    )

{-# INLINEABLE isScriptInvoked #-}
isScriptInvoked :: Credential -> DMap.Map Credential Lovelace -> Bool
isScriptInvoked = DMap.member

{-# INLINEABLE isRewardingScriptInfo #-}
isRewardingScriptInfo :: ScriptInfo -> Bool
isRewardingScriptInfo si = BI.fst (BI.unsafeDataAsConstr (toBuiltinData si)) `Builtins.equalsInteger` 2

{-# INLINEABLE isSpendingPurpose #-}
isSpendingPurpose :: ScriptPurpose -> Bool
isSpendingPurpose sp = BI.fst (BI.unsafeDataAsConstr (toBuiltinData sp)) `Builtins.equalsInteger` 1

{-# INLINEABLE hasCsFirstNonAda #-}
hasCsFirstNonAda :: CurrencySymbol -> Value -> Bool
hasCsFirstNonAda cs (Value m) =
  let bl = DMap.toBuiltinList m
   in Builtins.matchList'
        bl
        False
        ( \_ rest ->
            Builtins.matchList'
              rest
              False
              (\second _ -> unsafeFromBuiltinData (BI.fst second) == cs)
        )

{-# INLINEABLE valueFromCred #-}
valueFromCred
  :: Credential
  -> DList.List PubKeyHash
  -> DMap.Map Credential Lovelace
  -> DList.List TxInInfo
  -> Value
valueFromCred cred sigs wdrl inputs =
  Value (DMap.unsafeFromBuiltinList (go nilPairs (dlistToBI inputs)))
 where
  go acc xs =
    Builtins.matchList' xs acc $ \inp rest ->
      let TxInInfo _ txOut = unsafeFromBuiltinData inp
          TxOut{txOutAddress = addr, txOutValue = val} = txOut
          Address paymentCred mStake = addr
       in if paymentCred == cred
            then case mStake of
              Just (StakingHash ownerCred) ->
                case ownerCred of
                  PubKeyCredential pkh ->
                    if pkhElem pkh sigs
                      then go (biCurrencyUnion acc (stripAdaBI (toBuiltinData val))) rest
                      else traceError "Missing required pk witness"
                  ScriptCredential _ ->
                    if isScriptInvoked ownerCred wdrl
                      then go (biCurrencyUnion acc (stripAdaBI (toBuiltinData val))) rest
                      else traceError "Missing required script witness"
              _ -> error ()
            else go acc rest

{-# INLINEABLE valueToCred #-}
valueToCred :: Credential -> DList.List TxOut -> Value
valueToCred cred outputs =
  Value (DMap.unsafeFromBuiltinList (go nilPairs (dlistToBI outputs)))
 where
  go acc xs =
    Builtins.matchList' xs acc $ \txOutData rest ->
      let TxOut{txOutAddress = addr, txOutValue = val} = unsafeFromBuiltinData txOutData
          Address paymentCred _ = addr
       in if paymentCred == cred
            then go (biCurrencyUnion acc (stripAdaBI (toBuiltinData val))) rest
            else go acc rest

{-# INLINEABLE outputsContainExpectedValueAtCred #-}
outputsContainExpectedValueAtCred :: Credential -> DList.List TxOut -> Value -> Bool
outputsContainExpectedValueAtCred progLogicCred txOutputs expectedValue =
  let expectedCsPairs = DMap.toBuiltinList (getValue expectedValue)
   in Builtins.matchList'
        expectedCsPairs
        True
        ( \firstCsPair restCsPairs ->
            Builtins.matchList
              restCsPairs
              ( const
                  ( let expCs = unsafeFromBuiltinData (BI.fst firstCsPair) :: CurrencySymbol
                        expTnPairs = BI.unsafeDataAsMap (BI.snd firstCsPair)
                     in Builtins.matchList'
                          expTnPairs
                          True
                          ( \firstTnPair restTnPairs ->
                              Builtins.matchList
                                restTnPairs
                                ( const
                                    ( hasAtLeastAssetInProgOutputs
                                        (BI.unsafeDataAsI (BI.snd firstTnPair))
                                        0
                                        expCs
                                        (unsafeFromBuiltinData (BI.fst firstTnPair))
                                        (dlistToBI txOutputs)
                                    )
                                )
                                (\_ _ -> multiAssetCheck expectedCsPairs)
                          )
                  )
              )
              (\_ _ -> multiAssetCheck expectedCsPairs)
        )
 where
  hasAtLeastAssetInProgOutputs reqQty curQty cs tn outs
    | curQty >= reqQty = True
    | otherwise =
        Builtins.matchList
          outs
          (const (curQty >= reqQty))
          ( \txOutData rest ->
              let TxOut{txOutAddress = addr, txOutValue = val} = unsafeFromBuiltinData txOutData
                  Address paymentCred _ = addr
               in if paymentCred == progLogicCred
                    then hasAtLeastAssetInProgOutputs reqQty (curQty + valueOf val cs tn) cs tn rest
                    else hasAtLeastAssetInProgOutputs reqQty curQty cs tn rest
          )

  multiAssetCheck expCsPairs =
    let actualValue = valueToCred progLogicCred txOutputs
     in checkAllExpectedCsPairs actualValue expCsPairs

  checkAllExpectedCsPairs actual xs =
    Builtins.matchList' xs True $ \csPair rest ->
      let cs = unsafeFromBuiltinData (BI.fst csPair) :: CurrencySymbol
          tnPairs = BI.unsafeDataAsMap (BI.snd csPair)
       in checkAllExpectedTnPairs actual cs tnPairs && checkAllExpectedCsPairs actual rest

  checkAllExpectedTnPairs :: Value -> CurrencySymbol -> BIPairs -> Bool
  checkAllExpectedTnPairs actual cs tnPairs =
    Builtins.matchList' tnPairs True $ \tnPair rest ->
      let tn = unsafeFromBuiltinData (BI.fst tnPair) :: TokenName
          qty = BI.unsafeDataAsI (BI.snd tnPair)
       in valueOf actual cs tn >= qty && checkAllExpectedTnPairs actual cs rest

{-# INLINEABLE findReferenceInputByCS #-}
findReferenceInputByCS :: CurrencySymbol -> DList.List TxInInfo -> ProgrammableLogicGlobalParamsD
findReferenceInputByCS cs refInputs =
  go (dlistToBI refInputs)
 where
  go xs =
    BI.unsafeCaseList
      ( \inp rest ->
          let TxInInfo _ txOut = unsafeFromBuiltinData inp
              TxOut{txOutValue = val, txOutDatum = dat} = txOut
           in if hasCsFirstNonAda cs val
                then
                  matchOutputDatum
                    dat
                    (traceError "protocol params datum missing")
                    (\_ -> traceError "protocol params datum missing")
                    (\(Datum d) -> unsafeFromBuiltinData d)
                else go rest
      )
      xs

{-# INLINEABLE decodeDirectoryNode #-}
decodeDirectoryNode :: BI.BuiltinData -> DirectorySetNodeD
decodeDirectoryNode txOutData =
  let TxOut{txOutDatum = dat} = unsafeFromBuiltinData txOutData
   in matchOutputDatum
        dat
        (traceError "directory node datum missing")
        (\_ -> traceError "directory node datum missing")
        (\(Datum d) -> unsafeFromBuiltinData d)

{-# INLINEABLE checkTransferLogicAndGetProgrammableValue #-}
checkTransferLogicAndGetProgrammableValue
  :: CurrencySymbol
  -> BIList
  -> DList.List Integer
  -> DMap.Map Credential Lovelace
  -> Credential
  -> Value
  -> Value
checkTransferLogicAndGetProgrammableValue directoryNodeCS refInputs proofList wdrl initialCachedTransferScript totalValue =
  let inputCsPairs = DMap.toBuiltinList (getValue totalValue)
   in Value (DMap.unsafeFromBuiltinList (biReversePairs (go proofList inputCsPairs nilPairs initialCachedTransferScript)))
 where
  go :: DList.List Integer -> BIPairs -> BIPairs -> Credential -> BIPairs
  go proofs csPairs acc cachedTransferScript =
    Builtins.matchList' csPairs acc $ \currPair csPairsRest ->
      DList.caseList
        (\_ -> traceError "transfer proof missing")
        ( \proofIdx proofsRest ->
            let currCS = unsafeFromBuiltinData (BI.fst currPair) :: CurrencySymbol
                TxInInfo _ dirNodeTxOut = unsafeFromBuiltinData (biIndex refInputs proofIdx)
                TxOut{txOutValue = dirNodeValue} = dirNodeTxOut
                DirectorySetNodeD
                  { keyD = nodeKey
                  , nextD = nodeNext
                  , transferLogicScriptD = dirNodeTransferLogic
                  } = decodeDirectoryNode (toBuiltinData dirNodeTxOut)
             in if nodeKey < currCS
                  then
                    if currCS < nodeNext && hasCsFirstNonAda directoryNodeCS dirNodeValue
                      then go proofsRest csPairsRest acc cachedTransferScript
                      else traceError "dir neg-proof node must cover"
                  else
                    let transferScriptOk =
                          dirNodeTransferLogic
                            == cachedTransferScript
                            || isScriptInvoked dirNodeTransferLogic wdrl
                        keyMatch = nodeKey == currCS
                        validDirNode = hasCsFirstNonAda directoryNodeCS dirNodeValue
                     in if transferScriptOk && keyMatch && validDirNode
                          then go proofsRest csPairsRest (BI.mkCons currPair acc) dirNodeTransferLogic
                          else
                            if not transferScriptOk
                              then traceError "Missing required transfer script"
                              else
                                if not keyMatch
                                  then traceError "directory proof mismatch"
                                  else traceError "invalid dir node"
        )
        proofs

{-# INLINEABLE checkMintLogicAndGetProgrammableValue #-}
checkMintLogicAndGetProgrammableValue
  :: CurrencySymbol
  -> BIList
  -> DList.List Integer
  -> DMap.Map Credential Lovelace
  -> Value
  -> Value
checkMintLogicAndGetProgrammableValue directoryNodeCS refInputs proofList wdrl totalMintValue =
  let mintedEntries = DMap.toBuiltinList (getValue totalMintValue)
   in Value (DMap.unsafeFromBuiltinList (biReversePairs (go proofList mintedEntries nilPairs)))
 where
  go :: DList.List Integer -> BIPairs -> BIPairs -> BIPairs
  go proofs mintPairs progMintValue =
    Builtins.matchList
      mintPairs
      ( const
          ( if DList.null proofs
              then progMintValue
              else traceError "extra mint proof"
          )
      )
      ( \currPair mintRest ->
          DList.caseList
            (\_ -> traceError "mint proof missing")
            ( \proofIdx proofsRest ->
                let mintCs = unsafeFromBuiltinData (BI.fst currPair) :: CurrencySymbol
                    TxInInfo _ dirNodeTxOut = unsafeFromBuiltinData (biIndex refInputs proofIdx)
                    TxOut{txOutValue = dirNodeValue} = dirNodeTxOut
                    DirectorySetNodeD
                      { keyD = nodeKey
                      , nextD = nodeNext
                      , transferLogicScriptD = dirNodeTransferLogic
                      } = decodeDirectoryNode (toBuiltinData dirNodeTxOut)
                 in if nodeKey == mintCs
                      then
                        let transferScriptOk = isScriptInvoked dirNodeTransferLogic wdrl
                            validDirNode = hasCsFirstNonAda directoryNodeCS dirNodeValue
                         in if transferScriptOk && validDirNode
                              then go proofsRest mintRest (BI.mkCons currPair progMintValue)
                              else
                                if not transferScriptOk
                                  then traceError "Missing required transfer script"
                                  else traceError "invalid dir node m"
                      else
                        let coverLower = nodeKey < mintCs
                            coverUpper = mintCs < nodeNext
                            validDirNode = hasCsFirstNonAda directoryNodeCS dirNodeValue
                         in if coverLower && coverUpper && validDirNode
                              then go proofsRest mintRest progMintValue
                              else traceError "dir mint neg-proof node must cover"
            )
            proofs
      )

{-# INLINEABLE valueEqualsDeltaCurrencySymbol #-}
valueEqualsDeltaCurrencySymbol :: CurrencySymbol -> Value -> Value -> BIPairs
valueEqualsDeltaCurrencySymbol progCS inputValue outputValue =
  let innerInput = DMap.toBuiltinList (getValue inputValue)
      innerOutput = DMap.toBuiltinList (getValue outputValue)
   in goOuter innerInput innerOutput
 where
  goOuter :: BIPairs -> BIPairs -> BIPairs
  goOuter inPairs outPairs =
    Builtins.matchList' inPairs nilPairs $ \inH inR ->
      Builtins.matchList' outPairs nilPairs $ \outH outR ->
        let inCS = unsafeFromBuiltinData (BI.fst inH) :: CurrencySymbol
            outCS = unsafeFromBuiltinData (BI.fst outH) :: CurrencySymbol
         in if inCS == outCS
              then
                if inCS == progCS
                  then
                    if dataEqual
                      (Value (DMap.unsafeFromBuiltinList inR))
                      (Value (DMap.unsafeFromBuiltinList outR))
                      then
                        biSubtractTokens (BI.unsafeDataAsMap (BI.snd inH)) (BI.unsafeDataAsMap (BI.snd outH))
                      else
                        error ()
                  else
                    if BI.snd inH == BI.snd outH
                      then
                        goOuter inR outR
                      else
                        error ()
              else
                if BI.snd inH == BI.snd outH
                  then nilPairs
                  else error ()

{-# INLINEABLE processThirdPartyTransfer #-}
processThirdPartyTransfer
  :: CurrencySymbol
  -> Credential
  -> BIList
  -> BIList
  -> DList.List Integer
  -> BIPairs
  -> Bool
processThirdPartyTransfer programmableCS progLogicCred inputs progOutputs inputIdxs mintedTokens =
  go inputIdxs inputs progOutputs nilPairs
 where
  progLogicCredData :: BI.BuiltinData
  progLogicCredData = toBuiltinData progLogicCred

  go :: DList.List Integer -> BIList -> BIList -> BIPairs -> Bool
  go idxs remainingInputs programmableOutputs deltaAccumulator =
    DList.caseList
      ( \_ ->
          let finalDelta = biTokenUnion deltaAccumulator mintedTokens
              outputAccumulator = accumulateProgOutputTokens programmableOutputs
           in biTokenPairsContain outputAccumulator finalDelta || error ()
      )
      ( \relIdx restIdxs ->
          let dropped = biDrop relIdx remainingInputs
           in BI.unsafeCaseList
                ( \programmableInputData afterIdx ->
                    let TxInInfo _ inputTxOut = unsafeFromBuiltinData programmableInputData
                     in checkCorrespondingPair inputTxOut restIdxs afterIdx programmableOutputs deltaAccumulator
                )
                dropped
      )
      idxs

  checkCorrespondingPair :: TxOut -> DList.List Integer -> BIList -> BIList -> BIPairs -> Bool
  checkCorrespondingPair inputTxOut remainingIdxs remainingInputsAfterIdx programmableOutputs deltaAccumulator =
    let TxOut{txOutAddress = inputAddr, txOutValue = inputVal, txOutDatum = inputDat, txOutReferenceScript = inputRefScript} = inputTxOut
        Address inputCred _ = inputAddr
     in if toBuiltinData inputCred == progLogicCredData
          then
            BI.unsafeCaseList
              ( \outputTxOutData outputsRest ->
                  let TxOut{txOutAddress = outputAddr, txOutValue = outputVal, txOutDatum = outputDat, txOutReferenceScript = outputRefScript} = unsafeFromBuiltinData outputTxOutData
                   in if inputAddr == outputAddr && inputDat == outputDat && inputRefScript == outputRefScript
                        then
                          let delta = valueEqualsDeltaCurrencySymbol programmableCS inputVal outputVal
                           in go remainingIdxs remainingInputsAfterIdx outputsRest (biTokenUnion delta deltaAccumulator)
                        else
                          if inputAddr /= outputAddr
                            then traceError "corresponding output: address mismatch"
                            else traceError "corresponding output: datum/reference script mismatch"
              )
              programmableOutputs
          else case inputCred of
            ScriptCredential _ -> go remainingIdxs remainingInputsAfterIdx programmableOutputs deltaAccumulator
            _ -> traceError "input index points to pubkey input"

  accumulateProgOutputTokens :: BIList -> BIPairs
  accumulateProgOutputTokens xs =
    Builtins.matchList' xs nilPairs $ \txOutData rest ->
      let TxOut{txOutAddress = addr, txOutValue = val} = unsafeFromBuiltinData txOutData
          Address paymentCred _ = addr
       in if toBuiltinData paymentCred == progLogicCredData
            then biTokenUnion (withCurrencySymbol programmableCS val nilPairs DMap.toBuiltinList) (accumulateProgOutputTokens rest)
            else accumulateProgOutputTokens rest

{-# INLINEABLE enforceNSpendRedeemers #-}
enforceNSpendRedeemers :: Integer -> DMap.Map ScriptPurpose Redeemer -> Bool
enforceNSpendRedeemers n rdmrs =
  let bl = dropBuiltinList (n - 1) (DMap.toBuiltinList rdmrs)
   in Builtins.matchList'
        bl
        False
        ( \pair rest ->
            isSpendingPurpose (unsafeFromBuiltinData (BI.fst pair))
              && Builtins.matchList'
                rest
                True
                (\nextPair _ -> not (isSpendingPurpose (unsafeFromBuiltinData (BI.fst nextPair))))
        )
 where
  dropBuiltinList :: Integer -> BIPairs -> BIPairs
  dropBuiltinList m xs
    | m <= 0 = xs
    | otherwise = Builtins.matchList' xs xs (\_ ys -> dropBuiltinList (m - 1) ys)

{-# INLINEABLE mkProgrammableLogicBaseValidator #-}
mkProgrammableLogicBaseValidator :: BuiltinData -> BuiltinData -> ()
mkProgrammableLogicBaseValidator stakeCredData ctxData =
  let stakeCred = PlutusTx.unsafeFromBuiltinData @Credential stakeCredData
      ScriptContext{scriptContextTxInfo = txInfo_} =
        PlutusTx.unsafeFromBuiltinData @ScriptContext ctxData
      TxInfo{txInfoWdrl = wdrl} = txInfo_
   in if DMap.member stakeCred wdrl
        then ()
        else traceError "programmable global not invoked"

{-# INLINEABLE mkProgrammableLogicGlobalValidator #-}
mkProgrammableLogicGlobalValidator :: BuiltinData -> BuiltinData -> ()
mkProgrammableLogicGlobalValidator protocolParamsCsData ctxData =
  let protocolParamsCS = PlutusTx.unsafeFromBuiltinData @CurrencySymbol protocolParamsCsData
      ScriptContext
        { scriptContextTxInfo = txInfo_
        , scriptContextScriptInfo = scriptInfo_
        , scriptContextRedeemer = redeemer_
        } = PlutusTx.unsafeFromBuiltinData @ScriptContext ctxData
      TxInfo
        { txInfoInputs = inputs
        , txInfoReferenceInputs = referenceInputs
        , txInfoOutputs = outputs
        , txInfoMint = mint
        , txInfoWdrl = wdrl
        , txInfoSignatories = sigs
        , txInfoRedeemers = redeemers
        } = txInfo_
      red = PlutusTx.unsafeFromBuiltinData @ProgrammableLogicGlobalRedeemer (getRedeemer redeemer_)

      ProgrammableLogicGlobalParamsD
        { directoryNodeCSD = directoryNodeCS
        , progLogicCredD = progLogicCred
        } = findReferenceInputByCS protocolParamsCS referenceInputs

      refInputsBIList = dlistToBI referenceInputs
      mintValueAsValue = PlutusTx.unsafeFromBuiltinData @Value (PlutusTx.toBuiltinData mint)
   in case red of
        TransferAct{plgrTransferProofs, plgrMintProofs} ->
          let cachedTransferScript0 =
                BI.unsafeCaseList
                  (\first _ -> unsafeFromBuiltinData (BI.fst first))
                  (DMap.toBuiltinList wdrl)

              totalProgTokenValue_ =
                checkTransferLogicAndGetProgrammableValue
                  directoryNodeCS
                  refInputsBIList
                  plgrTransferProofs
                  wdrl
                  cachedTransferScript0
                  (valueFromCred progLogicCred sigs wdrl inputs)

              expectedProgrammableOutputValue =
                if DMap.null (getValue mintValueAsValue)
                  then totalProgTokenValue_
                  else
                    valueUnion
                      totalProgTokenValue_
                      ( checkMintLogicAndGetProgrammableValue
                          directoryNodeCS
                          refInputsBIList
                          plgrMintProofs
                          wdrl
                          mintValueAsValue
                      )
           in if isRewardingScriptInfo scriptInfo_
                && outputsContainExpectedValueAtCred
                  progLogicCred
                  outputs
                  expectedProgrammableOutputValue
                then ()
                else traceError "prog tokens escape"
        SeizeAct{plgrDirectoryNodeIdx, plgrInputIdxs, plgrOutputsStartIdx, plgrLengthInputIdxs} ->
          let remainingOutputs = biDrop plgrOutputsStartIdx (dlistToBI outputs)
              TxInInfo _ dirNodeTxOut = unsafeFromBuiltinData (biIndex refInputsBIList plgrDirectoryNodeIdx)
              TxOut{txOutValue = dirNodeValue} = dirNodeTxOut
              DirectorySetNodeD
                { keyD = programmableCS
                , issuerLogicScriptD = dirNodeIssuerLogic
                } = decodeDirectoryNode (toBuiltinData dirNodeTxOut)

              seizeMintedTokens = withCurrencySymbol programmableCS mintValueAsValue nilPairs DMap.toBuiltinList

              miniLedgerOk =
                processThirdPartyTransfer
                  programmableCS
                  progLogicCred
                  (dlistToBI inputs)
                  remainingOutputs
                  plgrInputIdxs
                  seizeMintedTokens
              issuerLogicOk = isScriptInvoked dirNodeIssuerLogic wdrl
              validDirNode = hasCsFirstNonAda directoryNodeCS dirNodeValue
              spendRedeemersOk = enforceNSpendRedeemers plgrLengthInputIdxs redeemers
              inputIdxLenOk = DList.length plgrInputIdxs == plgrLengthInputIdxs
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

plinthProgrammableLogicBaseScript :: Script
plinthProgrammableLogicBaseScript =
  compiledCodeToScript $ plinthc mkProgrammableLogicBaseValidator

plinthProgrammableLogicGlobalScript :: Script
plinthProgrammableLogicGlobalScript =
  compiledCodeToScript $ plinthc mkProgrammableLogicGlobalValidator
