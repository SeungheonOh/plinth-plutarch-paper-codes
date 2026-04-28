{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module SmartTokens.Contracts.ProgrammableLogicBase (
  ProgrammableLogicGlobalRedeemer (..),
  absoluteToRelativeInputIdxs,
  mkSeizeActRedeemerFromAbsoluteInputIdxs,
  mkSeizeActRedeemerFromRelativeInputIdxs,
  mkProgrammableLogicBase,
  mkProgrammableLogicGlobal,
  pisScriptInvokedEntries,
  pvalueFromCred,
  pvalueToCred,
  poutputsContainExpectedValueAtCred,
  pscriptContextTxInfo,
) where

import Data.Kind (Type)
import Data.List (foldl')
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Integer (pconstantInteger)
import Plutarch.Internal.Lift ()
import Plutarch.Internal.Term (punsafeBuiltin)
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore qualified as PLC
import PlutusLedgerApi.V1.Value (Value)
import PlutusTx qualified
import SmartTokens.Types.PTokenDirectory (PDirectorySetNode (..))
import SmartTokens.Types.ProtocolParams (PProgrammableLogicGlobalParams (..))

-- ============================================================================
-- 1. Plutarch infrastructure
-- ============================================================================
-- Plinth: compiledCodeToScript, dataEqual

type PType = S -> Type
type ClosedTerm (a :: PType) = forall (s :: S). Term s a

pmapData :: Term s (PBuiltinList (PBuiltinPair PData PData) :--> PData)
pmapData = punsafeBuiltin PLC.MapData

ppairDataBuiltinRaw :: Term s (PData :--> PData :--> PBuiltinPair PData PData)
ppairDataBuiltinRaw = punsafeBuiltin PLC.MkPairData

-- | Unsafely unwrap a PMaybeData known to be Just.
pjustData :: Term s (PMaybeData a) -> Term s a
pjustData term =
  punsafeCoerce $ phead # (psndBuiltin # (pasConstr # pforgetData (pdata term)))

pand'List :: [Term s PBool] -> Term s PBool
pand'List ts' = case ts' of
  [] -> pconstant True
  ts -> foldl1 (\res x -> pand' # res # x) ts

pvalidateConditions :: [Term s PBool] -> Term s PUnit
pvalidateConditions conds = pif (pand'List conds) (pconstant ()) perror

-- ============================================================================
-- 2. List operations
-- ============================================================================
-- Plinth: dropList, listLength, appendList, pkhElem, isNullList, indexList

pnTails :: (PIsListLike list a) => Integer -> Term s (list a) -> Term s (list a)
pnTails n xs = foldl' (\acc _ -> ptail # acc) xs (replicate (fromIntegral n) ())

ptails10 :: (PIsListLike list a) => ClosedTerm (list a :--> list a)
ptails10 = phoistAcyclic $ plam (pnTails 10)

ptails20 :: (PIsListLike list a) => ClosedTerm (list a :--> list a)
ptails20 = phoistAcyclic $ plam (\xs -> ptails10 # (ptails10 # xs))

ptails30 :: (PIsListLike list a) => ClosedTerm (list a :--> list a)
ptails30 = phoistAcyclic $ plam (\xs -> ptails20 # (ptails10 # xs))

pdropR :: forall (list :: PType -> PType) (a :: PType) (s :: S). (PIsListLike list a) => Term s (PInteger :--> list a :--> list a)
pdropR = phoistAcyclic $ pfix #$ plam $ \self n ys -> pif (n #== 0) ys (self # (n - 1) # (ptail # ys))

pdropFast :: (PIsListLike PBuiltinList a) => Term s (PInteger :--> PBuiltinList a :--> PBuiltinList a)
pdropFast = phoistAcyclic $
  pfix #$ plam $ \self n ys ->
    pcond
      [ (30 #<= n, self # (n - 30) # (ptails30 # ys))
      , (20 #<= n, self # (n - 20) # (ptails20 # ys))
      , (10 #<= n, self # (n - 10) # (ptails10 # ys))
      ]
      (pdropR # n # ys)

pbuiltinListLength :: forall s a. (PElemConstraint PBuiltinList a) => Term s PInteger -> Term s (PBuiltinList a :--> PInteger)
pbuiltinListLength acc =
  ( pfix #$ plam $ \self acc' l ->
      pelimList (\_ ys -> self # (acc' + 1) # ys) acc' l
  )
    # acc

pbuiltinListLengthFast :: forall (a :: PType) (s :: S). (PElemConstraint PBuiltinList a) => Term s (PInteger :--> PBuiltinList a :--> PInteger)
pbuiltinListLengthFast = phoistAcyclic $ plam $ \n elems ->
  let go :: Term _ (PInteger :--> PInteger :--> PBuiltinList a :--> PInteger)
      go = pfix #$ plam $ \self remainingExpected currentCount xs ->
        pcond
          [ (30 #<= remainingExpected, self # (remainingExpected - 30) # (currentCount + 30) # (ptails30 # xs))
          , (20 #<= remainingExpected, self # (remainingExpected - 20) # (currentCount + 20) # (ptails20 # xs))
          , (10 #<= remainingExpected, self # (remainingExpected - 10) # (currentCount + 10) # (ptails10 # xs))
          ]
          (currentCount + pbuiltinListLength 0 # xs)
   in go # n # 0 # elems

-- ============================================================================
-- 3. Value helpers
-- ============================================================================
-- Plinth: valueToSortedList, sortedListToValue, emptyValue, stripAda,
--         tokenPairsUnion, currencyPairsUnion, valueUnion, assetQtyInValue,
--         tokensForCurrencySymbol, negateTokens, subtractTokens, tokenPairsContain

emptyValue :: Value
emptyValue = mempty

pemptyLedgerValue :: Term s (PValue 'Sorted 'Positive)
pemptyLedgerValue = punsafeCoerce $ pconstant @(PValue 'Unsorted 'NoGuarantees) emptyValue

-- | Strip Ada (first entry) from a ledger-provided sorted value.
pstripAdaH
  :: forall (v :: AmountGuarantees) (s :: S)
   . Term s (PValue 'Sorted v) -> Term s (PValue 'Sorted v)
pstripAdaH value =
  let nonAdaValueMapInner = ptail # pto (pto value)
   in pcon (PValue $ pcon $ PMap nonAdaValueMapInner)

-- | Merge two sorted token-name maps by asset-wise addition.
ptokenPairsUnionFast
  :: Term
       s
       ( PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
       )
ptokenPairsUnionFast = phoistAcyclic $
  pfix #$ plam $ \self tokensA tokensB ->
    pelimList
      ( \tokenPairA tokensARest ->
          pelimList
            ( \tokenPairB tokensBRest ->
                let tokenNameA = pfstBuiltin # tokenPairA
                    tokenNameB = pfstBuiltin # tokenPairB
                    tokenNameABytes = pasByteStr # pforgetData tokenNameA
                    tokenNameBBytes = pasByteStr # pforgetData tokenNameB
                 in pif
                      (tokenNameABytes #== tokenNameBBytes)
                      ( let quantityA = pfromData (psndBuiltin # tokenPairA)
                            quantityB = pfromData (psndBuiltin # tokenPairB)
                         in pcons
                              # (ppairDataBuiltin # tokenNameA # pdata (quantityA + quantityB))
                              # (self # tokensARest # tokensBRest)
                      )
                      ( pif
                          (tokenNameABytes #< tokenNameBBytes)
                          (pcons # tokenPairA # (self # tokensARest # tokensB))
                          (pcons # tokenPairB # (self # tokensA # tokensBRest))
                      )
            )
            tokensA
            tokensB
      )
      tokensB
      tokensA

-- | Merge two sorted currency-symbol maps by asset-wise addition.
pcurrencyPairsUnionFast
  :: Term
       s
       ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
           :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
           :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
       )
pcurrencyPairsUnionFast = phoistAcyclic $
  pfix #$ plam $ \self csPairsA csPairsB ->
    pelimList
      ( \csPairA csPairsARest ->
          pelimList
            ( \csPairB csPairsBRest ->
                let currencySymbolA = pfstBuiltin # csPairA
                    currencySymbolB = pfstBuiltin # csPairB
                    currencySymbolABytes = pasByteStr # pforgetData currencySymbolA
                    currencySymbolBBytes = pasByteStr # pforgetData currencySymbolB
                 in pif
                      (currencySymbolABytes #== currencySymbolBBytes)
                      ( let tokenPairsA = pto (pfromData (psndBuiltin # csPairA))
                            tokenPairsB = pto (pfromData (psndBuiltin # csPairB))
                            mergedTokenPairs = ptokenPairsUnionFast # tokenPairsA # tokenPairsB
                            mergedPair =
                              punsafeCoerce $
                                ppairDataBuiltinRaw
                                  # pforgetData currencySymbolA
                                  # (pmapData # punsafeCoerce mergedTokenPairs)
                         in pcons
                              # mergedPair
                              # (self # csPairsARest # csPairsBRest)
                      )
                      ( pif
                          (currencySymbolABytes #< currencySymbolBBytes)
                          (pcons # csPairA # (self # csPairsARest # csPairsB))
                          (pcons # csPairB # (self # csPairsA # csPairsBRest))
                      )
            )
            csPairsA
            csPairsB
      )
      csPairsB
      csPairsA

-- | Add two sorted non-Ada Values.
pvalueUnionFast :: Term s (PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive)
pvalueUnionFast = phoistAcyclic $ plam $ \valueA valueB ->
  pcon $
    PValue $
      pcon $
        PMap $
          pcurrencyPairsUnionFast
            # pto (pto valueA)
            # pto (pto valueB)

-- | Get the quantity of a specific asset in a value.
passetQtyInValue
  :: Term
       s
       ( PValue 'Sorted 'Positive
           :--> PCurrencySymbol
           :--> PTokenName
           :--> PInteger
       )
passetQtyInValue = phoistAcyclic $ plam $ \value cs tn ->
  let tokenQtyInTokenPairs = pfix #$ plam $ \self remainingTokenPairs ->
        pelimList
          ( \tokenPair tokenPairsRest ->
              let tokenName = pfromData (pfstBuiltin # tokenPair)
                  tokenQty = pfromData (psndBuiltin # tokenPair)
               in pif
                    (tokenName #== tn)
                    tokenQty
                    ( pif
                        (tn #< tokenName)
                        0
                        (self # tokenPairsRest)
                    )
          )
          0
          remainingTokenPairs
      tokenQtyInCurrencyPairs = pfix #$ plam $ \self remainingCurrencyPairs ->
        pelimList
          ( \currencyPair currencyPairsRest ->
              let currencySymbol = pfromData (pfstBuiltin # currencyPair)
                  tokenPairs = pto (pfromData (psndBuiltin # currencyPair))
               in pif
                    (currencySymbol #== cs)
                    (tokenQtyInTokenPairs # tokenPairs)
                    ( pif
                        (cs #< currencySymbol)
                        0
                        (self # currencyPairsRest)
                    )
          )
          0
          remainingCurrencyPairs
   in tokenQtyInCurrencyPairs # pto (pto value)

-- | Get token pairs for a specific currency symbol from a sorted value.
ptokensForCurrencySymbol
  :: forall anyAmount s
   . Term s (PCurrencySymbol :--> PValue 'Sorted anyAmount :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
ptokensForCurrencySymbol =
  phoistAcyclic $
    plam $ \targetCs mintValue ->
      let mintedEntries :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
          mintedEntries = pto (pto mintValue)
          go = pfix #$ plam $ \self remainingMintEntries ->
            pelimList
              ( \mintCsPair mintCsPairs ->
                  let mintCs = pfromData (pfstBuiltin # mintCsPair)
                   in pif
                        (mintCs #== targetCs)
                        (pto (pfromData (psndBuiltin # mintCsPair)))
                        (pif (targetCs #< mintCs) pnil (self # mintCsPairs))
              )
              pnil
              remainingMintEntries
       in go # mintedEntries

-- | Negate all quantities in a token-name map.
pnegateTokens :: Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
pnegateTokens = pfix #$ plam $ \self tokens ->
  pelimList
    ( \tokenPair tokensRest ->
        let tokenName = pfstBuiltin # tokenPair
            tokenAmount = psndBuiltin # tokenPair
         in pcons # (ppairDataBuiltin # tokenName # pdata (pconstantInteger 0 - pfromData tokenAmount)) # (self # tokensRest)
    )
    pnil
    tokens

-- | Subtract token pairs: input - output, skipping zero results.
psubtractTokens
  :: Term
       s
       ( PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
       )
psubtractTokens = phoistAcyclic $
  pfix #$ plam $ \self inputTokens outputTokens ->
    pelimList
      ( \inputPair inputRest ->
          plet (pfstBuiltin # inputPair) $ \inputTokenName ->
            let inputTokenAmount = psndBuiltin # inputPair
             in pelimList
                  ( \outputPair outputRest ->
                      let outputTokenName = pfstBuiltin # outputPair
                          outputTokenAmount = psndBuiltin # outputPair
                       in pif
                            (pfromData inputTokenName #<= pfromData outputTokenName)
                            ( pif
                                (inputTokenName #== outputTokenName)
                                ( let diff = pfromData inputTokenAmount - pfromData outputTokenAmount
                                   in pif
                                        (diff #== 0)
                                        (self # inputRest # outputRest)
                                        ( pcons
                                            # (ppairDataBuiltin # inputTokenName # pdata diff)
                                            # (self # inputRest # outputRest)
                                        )
                                )
                                ( let diff = pfromData inputTokenAmount
                                   in pcons
                                        # (ppairDataBuiltin # inputTokenName # pdata diff)
                                        # (self # inputRest # outputTokens)
                                )
                            )
                            ( let diff = pconstantInteger 0 - pfromData outputTokenAmount
                               in pcons
                                    # (ppairDataBuiltin # outputTokenName # pdata diff)
                                    # (self # inputTokens # outputRest)
                            )
                  )
                  inputRest
                  outputTokens
      )
      (pnegateTokens # outputTokens)
      inputTokens

-- | Check that actualTokens contain at least requiredTokens quantities.
ptokenPairsContain
  :: Term
       s
       ( PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBool
       )
ptokenPairsContain = phoistAcyclic $
  pfix #$ plam $ \self actualTokens requiredTokens ->
    pelimList
      ( \requiredPair requiredRest ->
          let requiredTokenName = pfromData (pfstBuiltin # requiredPair)
              requiredQty = pfromData (psndBuiltin # requiredPair)
           in pelimList
                ( \actualPair actualRest ->
                    let actualTokenName = pfromData (pfstBuiltin # actualPair)
                        actualQty = pfromData (psndBuiltin # actualPair)
                     in pif
                          (actualTokenName #== requiredTokenName)
                          (pif (actualQty #>= requiredQty) (self # actualRest # requiredRest) (pconstant False))
                          ( pif
                              (actualTokenName #< requiredTokenName)
                              (self # actualRest # requiredTokens)
                              (pif (0 #>= requiredQty) (self # actualTokens # requiredRest) (pconstant False))
                          )
                )
                (pif (0 #>= requiredQty) (self # pnil # requiredRest) (pconstant False))
                actualTokens
      )
      (pconstant True)
      requiredTokens

-- ============================================================================
-- 4. Credential and address helpers
-- ============================================================================
-- Plinth: getPaymentCredential, getStakingCredential

paddressCredential :: Term s PAddress -> Term s PCredential
paddressCredential addr = pmatch addr $ \addr' -> paddress'credential addr'

pscriptContextTxInfo :: Term s PScriptContext -> Term s PTxInfo
pscriptContextTxInfo ctx = pmatch ctx $ \ctx' -> pscriptContext'txInfo ctx'

ptxInInfoResolved :: Term s PTxInInfo -> Term s PTxOut
ptxInInfoResolved txInInfo = pmatch txInInfo $ \txInInfo' -> ptxInInfo'resolved txInInfo'

ptxOutDatum :: Term s PTxOut -> Term s POutputDatum
ptxOutDatum = flip pmatch $ \txo -> ptxOut'datum txo

ptxOutValue :: Term s PTxOut -> Term s (PAsData (PValue 'Sorted 'Positive))
ptxOutValue = flip pmatch $ \txo -> ptxOut'value txo

-- ============================================================================
-- 5. Withdrawal and script info checking
-- ============================================================================
-- Plinth: isScriptInvokedEntries, isRewardingScriptInfo, isSpendingPurpose

-- | Check whether a credential appears in withdrawals.
pisScriptInvokedEntries :: Term s (PAsData PCredential :--> PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)) :--> PBool)
pisScriptInvokedEntries = phoistAcyclic $ plam $ \scriptCredData withdrawalEntries ->
  let go = pfix #$ plam $ \self entries ->
        let entry = phead # entries
         in (pfstBuiltin # entry)
              #== scriptCredData
              #|| plet
                (ptail # entries)
                ( \entries' ->
                    let entryA = phead # entries'
                     in (pfstBuiltin # entryA) #== scriptCredData #|| self # entries'
                )
   in go # withdrawalEntries

-- | Check if ScriptInfo is a rewarding script.
pisRewardingScript :: Term s (PAsData PScriptInfo) -> Term s PBool
pisRewardingScript term = (pfstBuiltin # (pasConstr # pforgetData term)) #== 2

-- | Check if tx is signed by a pubkey hash.
ptxSignedByPkh :: Term s (PAsData PPubKeyHash :--> PBuiltinList (PAsData PPubKeyHash) :--> PBool)
ptxSignedByPkh = pelem

-- ============================================================================
-- 6. Value CS checking
-- ============================================================================
-- Plinth: hasCsFirstNonAda, hasCsFirstNonAdaOrFalse

-- | Check that the first non-Ada policy matches a state-token CS.
phasCSH :: Term s PCurrencySymbol -> Term s (PAsData (PValue 'Sorted 'Positive)) -> Term s PBool
phasCSH directoryNodeCS value =
  let value' = pto (pto (pfromData value))
   in pfromData (pfstBuiltin # (phead # (ptail # value'))) #== directoryNodeCS

-- | Safe variant that returns False instead of crashing on missing non-Ada entries.
phasCSHOrFalse :: Term s PCurrencySymbol -> Term s (PAsData (PValue 'Sorted 'Positive)) -> Term s PBool
phasCSHOrFalse directoryNodeCS value =
  let nonAdaEntries = ptail # pto (pto (pfromData value))
   in pelimList
        (\currencyPair _ -> pfromData (pfstBuiltin # currencyPair) #== directoryNodeCS)
        (pcon PFalse)
        nonAdaEntries

-- ============================================================================
-- 7. Value aggregation
-- ============================================================================
-- Plinth: valueFromCred, valueToCred

-- | Aggregate all non-Ada input value controlled by a payment credential.
pvalueFromCred
  :: Term s PCredential
  -> Term s (PBuiltinList (PAsData PPubKeyHash))
  -> Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))
  -> Term s (PBuiltinList (PAsData PTxInInfo))
  -> Term s (PValue 'Sorted 'Positive)
pvalueFromCred cred sigs withdrawalEntries inputs =
  let credData = pforgetData (pdata cred)
   in ( pfix #$ plam $ \self acc ->
          pelimList
            ( \txIn xs ->
                plet (pdata (ptxInInfoResolved $ pfromData txIn)) $ \resolvedOutData ->
                  plet (psndBuiltin # (pasConstr # pforgetData resolvedOutData)) $ \resolvedOutFields ->
                    let resolvedOutAddressData = phead # resolvedOutFields
                        resolvedOutFieldsRest = ptail # resolvedOutFields
                        resolvedOutValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # resolvedOutFieldsRest)
                        paymentCredData = phead # (psndBuiltin # (pasConstr # resolvedOutAddressData))
                        stakingCredMaybe = punsafeCoerce @(PMaybeData PStakingCredential) (phead # (ptail # (psndBuiltin # (pasConstr # resolvedOutAddressData))))
                     in self
                          # pif
                            (paymentCredData #== credData)
                            ( pmatch (pjustData stakingCredMaybe) $ \case
                                PStakingHash ownerCred ->
                                  pmatch ownerCred $ \case
                                    PPubKeyCredential pkh ->
                                      pif
                                        (ptxSignedByPkh # pkh # sigs)
                                        (pvalueUnionFast # acc # pstripAdaH (pfromData resolvedOutValue))
                                        (ptraceInfoError "Missing required pk witness")
                                    PScriptCredential scriptHash_ ->
                                      let scriptCredData = pdata $ pcon (PScriptCredential scriptHash_)
                                       in pif
                                            (pisScriptInvokedEntries # scriptCredData # withdrawalEntries)
                                            (pvalueUnionFast # acc # pstripAdaH (pfromData resolvedOutValue))
                                            (ptraceInfoError "Missing required script witness")
                                _ -> perror
                            )
                            acc
                          # xs
            )
            acc
      )
        # pemptyLedgerValue
        # inputs

-- | Aggregate all non-Ada output value at a payment credential.
pvalueToCred
  :: Term s PCredential
  -> Term s (PBuiltinList (PAsData PTxOut))
  -> Term s (PValue 'Sorted 'Positive)
pvalueToCred cred inputs =
  let credData = pforgetData (pdata cred)
   in ( pfix #$ plam $ \self acc ->
          pelimList
            ( \txOut xs ->
                plet (psndBuiltin # (pasConstr # pforgetData txOut)) $ \txOutFields ->
                  let txOutAddress = phead # txOutFields
                      txOutFieldsRest = ptail # txOutFields
                      txOutValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # txOutFieldsRest)
                      paymentCredData = phead # (psndBuiltin # (pasConstr # txOutAddress))
                   in self
                        # pif (paymentCredData #== credData) (pvalueUnionFast # acc # pstripAdaH (pfromData txOutValue)) acc
                        # xs
            )
            acc
      )
        # pemptyLedgerValue
        # inputs

-- ============================================================================
-- 8. Output containment check
-- ============================================================================
-- Plinth: outputsContainExpectedValueAtCred

poutputsContainExpectedValueAtCred
  :: Term s PCredential
  -> Term s (PBuiltinList (PAsData PTxOut))
  -> Term s (PValue 'Sorted 'Positive)
  -> Term s PBool
poutputsContainExpectedValueAtCred progLogicCred txOutputs expectedValue =
  let
    hasAtLeastAssetInProgOutputs = pfix #$ plam $ \self requiredQty currentQty cs tn remainingOutputs ->
      pif
        (currentQty #>= requiredQty)
        (pconstant True)
        ( pelimList
            ( \txOut outputsRest ->
                pmatch (pfromData txOut) $ \(PTxOut{ptxOut'address, ptxOut'value}) ->
                  pif
                    (paddressCredential ptxOut'address #== progLogicCred)
                    (self # requiredQty # (currentQty + (passetQtyInValue # pfromData ptxOut'value # cs # tn)) # cs # tn # outputsRest)
                    (self # requiredQty # currentQty # cs # tn # outputsRest)
            )
            (currentQty #>= requiredQty)
            remainingOutputs
        )
    checkExpectedTokenPairsAgainstActualValue = pfix #$ plam $ \self actualValue expectedCurrencySymbol remainingExpectedTokenPairs ->
      pelimList
        ( \expectedTokenPair expectedTokenPairsRest ->
            let expectedTokenName = pfromData (pfstBuiltin # expectedTokenPair)
                expectedTokenQty = pfromData (psndBuiltin # expectedTokenPair)
             in (passetQtyInValue # actualValue # expectedCurrencySymbol # expectedTokenName #>= expectedTokenQty)
                  #&& self
                  # actualValue
                  # expectedCurrencySymbol
                  # expectedTokenPairsRest
        )
        (pconstant True)
        remainingExpectedTokenPairs
    checkExpectedCurrencyPairsAgainstActualValue = pfix #$ plam $ \self actualValue remainingExpectedCurrencyPairs ->
      pelimList
        ( \expectedCurrencyPair expectedCurrencyPairsRest ->
            let expectedCurrencySymbol = pfromData (pfstBuiltin # expectedCurrencyPair)
                expectedTokenPairs = pto (pfromData (psndBuiltin # expectedCurrencyPair))
             in checkExpectedTokenPairsAgainstActualValue
                  # actualValue
                  # expectedCurrencySymbol
                  # expectedTokenPairs
                  #&& self
                  # actualValue
                  # expectedCurrencyPairsRest
        )
        (pconstant True)
        remainingExpectedCurrencyPairs
    expectedCsPairs = pto (pto expectedValue)
    actualValueAtCred = pvalueToCred progLogicCred txOutputs
   in
    pelimList
      ( \firstExpectedCsPair expectedCsPairsRest ->
          let firstExpectedTokenPairs = pto (pfromData (psndBuiltin # firstExpectedCsPair))
           in pelimList
                ( \expectedTokenPair firstExpectedTokenPairsRest ->
                    pif
                      (pnull # expectedCsPairsRest #&& pnull # firstExpectedTokenPairsRest)
                      ( let expectedCurrencySymbol = pfromData (pfstBuiltin # firstExpectedCsPair)
                            expectedTokenName = pfromData (pfstBuiltin # expectedTokenPair)
                            expectedRequiredQty = pfromData (psndBuiltin # expectedTokenPair)
                         in hasAtLeastAssetInProgOutputs
                              # expectedRequiredQty
                              # 0
                              # expectedCurrencySymbol
                              # expectedTokenName
                              # txOutputs
                      )
                      (checkExpectedCurrencyPairsAgainstActualValue # actualValueAtCred # expectedCsPairs)
                )
                (checkExpectedCurrencyPairsAgainstActualValue # actualValueAtCred # expectedCsPairs)
                firstExpectedTokenPairs
      )
      (pconstant True)
      expectedCsPairs

-- ============================================================================
-- 9. Reference input lookup
-- ============================================================================
-- Plinth: findReferenceInputByCS, decodeDirectoryNode

pfindReferenceInputByCS
  :: Term s PCurrencySymbol
  -> Term s (PBuiltinList (PAsData PTxInInfo))
  -> Term s PProgrammableLogicGlobalParams
pfindReferenceInputByCS currencySymbol referenceInputs =
  let extractParams resolvedOut =
        pmatch (ptxOutDatum resolvedOut) $ \case
          POutputDatum paramDat' ->
            pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalParams) (pto paramDat')
          _ -> ptraceInfoError "protocol params datum missing"
      go = pfix #$ plam $ \self remainingRefInputs ->
        let txIn = phead # remainingRefInputs
         in plet (ptxInInfoResolved $ pfromData txIn) $ \resolvedOut ->
              pif
                (phasCSHOrFalse currencySymbol (ptxOutValue resolvedOut))
                (extractParams resolvedOut)
                (self # (ptail # remainingRefInputs))
   in go # referenceInputs

-- ============================================================================
-- 10. Transfer logic validation
-- ============================================================================
-- Plinth: checkTransferLogicAndGetProgrammableValue

pcheckTransferLogicAndGetProgrammableValue
  :: Term s PCurrencySymbol
  -> Term s (PBuiltinList (PAsData PTxInInfo))
  -> Term s (PBuiltinList (PAsData PInteger))
  -> Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))
  -> Term s (PAsData PCredential)
  -> Term s (PValue 'Sorted 'Positive)
  -> Term s (PValue 'Sorted 'Positive)
pcheckTransferLogicAndGetProgrammableValue directoryNodeCS refInputs proofList withdrawalEntries initialCachedTransferScript totalValue =
  let mapInnerList :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
      mapInnerList = pto (pto totalValue)
      go = pfix #$ plam $ \self proofs inputInnerValue actualProgrammableTokenValue cachedTransferScript ->
        pelimList
          ( \csPair csPairs ->
              P.do
                PTxOut{ptxOut'value = directoryNodeUTxOFValue, ptxOut'datum = directoryNodeUTxOFDatum} <-
                  pmatch $ ptxInInfoResolved (pfromData $ phead # (pdropFast # pfromData (phead # proofs) # refInputs))
                POutputDatum directoryNodeDatum' <- pmatch directoryNodeUTxOFDatum
                PDirectorySetNode
                  { pkey = directoryNodeDatumFkey
                  , pnext = directoryNodeDatumFNext
                  , ptransferLogicScript = directoryNodeDatumFTransferLogicScript
                  } <-
                  pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto directoryNodeDatum'))
                let currCS = pfromData (pfstBuiltin # csPair)
                    nodeKey = pfromData directoryNodeDatumFkey
                    nodeNext = pfromData directoryNodeDatumFNext
                pif
                  (nodeKey #< currCS)
                  ( let checks =
                          pand'List
                            [ ptraceInfoIfFalse "dir neg-proof node must cover" (currCS #< nodeNext)
                            , ptraceInfoIfFalse "invalid dir node n" (phasCSH directoryNodeCS directoryNodeUTxOFValue)
                            ]
                     in pif
                          checks
                          ( self
                              # (ptail # proofs)
                              # csPairs
                              # actualProgrammableTokenValue
                              # cachedTransferScript
                          )
                          perror
                  )
                  ( let checks =
                          pand'List
                            [ ptraceInfoIfFalse "Missing required transfer script" $
                                (directoryNodeDatumFTransferLogicScript #== cachedTransferScript)
                                  #|| (pisScriptInvokedEntries # directoryNodeDatumFTransferLogicScript # withdrawalEntries)
                            , ptraceInfoIfFalse "directory proof mismatch" (nodeKey #== currCS)
                            , ptraceInfoIfFalse "invalid dir node" (phasCSH directoryNodeCS directoryNodeUTxOFValue)
                            ]
                     in pif
                          checks
                          ( self
                              # (ptail # proofs)
                              # csPairs
                              # (pcons # csPair # actualProgrammableTokenValue)
                              # directoryNodeDatumFTransferLogicScript
                          )
                          perror
                  )
          )
          (pcon $ PValue $ pcon $ PMap actualProgrammableTokenValue)
          inputInnerValue
   in go
        # proofList
        # mapInnerList
        # pto (pto pemptyLedgerValue)
        # initialCachedTransferScript

-- ============================================================================
-- 11. Mint logic validation
-- ============================================================================
-- Plinth: checkMintLogicAndGetProgrammableValue

pcheckMintLogicAndGetProgrammableValue
  :: Term s PCurrencySymbol
  -> Term s (PBuiltinList (PAsData PTxInInfo))
  -> Term s (PBuiltinList (PAsData PInteger))
  -> Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))
  -> Term s (PValue 'Sorted 'NoGuarantees)
  -> Term s (PValue 'Sorted 'NoGuarantees)
pcheckMintLogicAndGetProgrammableValue directoryNodeCS refInputs proofList withdrawalEntries totalMintValue =
  let mintedEntries :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
      mintedEntries = pto (pto totalMintValue)
      go = pfix #$ plam $ \self proofs remainingMintEntries programmableMintValue ->
        pelimList
          ( \mintCsPair mintCsPairs ->
              pelimList
                ( \nodeIdx proofsRest ->
                    let mintCs = pfstBuiltin # mintCsPair
                     in P.do
                          PTxOut{ptxOut'value = directoryNodeUTxOFValue, ptxOut'datum = directoryNodeUTxOFDatum} <-
                            pmatch $ ptxInInfoResolved (pfromData $ phead # (pdropFast # pfromData nodeIdx # refInputs))
                          POutputDatum paramDat' <- pmatch directoryNodeUTxOFDatum
                          PDirectorySetNode
                            { pkey = directoryNodeDatumFkey
                            , pnext = directoryNodeDatumFNext
                            , ptransferLogicScript = directoryNodeDatumFTransferLogicScript
                            } <-
                            pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto paramDat'))
                          let currCS = pfromData mintCs
                              nodeKey = pfromData directoryNodeDatumFkey
                              nodeNext = pfromData directoryNodeDatumFNext
                          pif
                            (nodeKey #== currCS)
                            ( let checks =
                                    pand'List
                                      [ ptraceInfoIfFalse "Missing required transfer script" (pisScriptInvokedEntries # directoryNodeDatumFTransferLogicScript # withdrawalEntries)
                                      , ptraceInfoIfFalse "invalid dir node m" (phasCSH directoryNodeCS directoryNodeUTxOFValue)
                                      ]
                               in pif
                                    checks
                                    (self # proofsRest # mintCsPairs # (pcons # mintCsPair # programmableMintValue))
                                    perror
                            )
                            ( let checks =
                                    pand'List
                                      [ ptraceInfoIfFalse "dir mint neg-proof node must cover" (nodeKey #< currCS)
                                      , ptraceInfoIfFalse "dir mint neg-proof node must cover" (currCS #< nodeNext)
                                      , ptraceInfoIfFalse "invalid dir node n" (phasCSH directoryNodeCS directoryNodeUTxOFValue)
                                      ]
                               in pif
                                    checks
                                    (self # proofsRest # mintCsPairs # programmableMintValue)
                                    perror
                            )
                )
                (ptraceInfoError "mint proof missing")
                proofs
          )
          (pelimList (\_ _ -> ptraceInfoError "extra mint proof") (pcon $ PValue $ pcon $ PMap programmableMintValue) proofs)
          remainingMintEntries
   in go # proofList # mintedEntries # pnil

-- ============================================================================
-- 12. Redeemer types and offchain helpers
-- ============================================================================
-- Plinth: imports ProgrammableLogicGlobalRedeemer from this module

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
-- 13. SeizeAct: value delta
-- ============================================================================
-- Plinth: valueEqualsDeltaCurrencySymbol, dataEqual

-- | Compare two values; require equality everywhere except one CS; return that CS's signed delta.
pvalueEqualsDeltaCurrencySymbol
  :: forall anyOrder anyAmount s
   . Term s PCurrencySymbol
  -> Term s (PAsData (PValue anyOrder anyAmount))
  -> Term s (PAsData (PValue anyOrder anyAmount))
  -> Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
pvalueEqualsDeltaCurrencySymbol progCS inputUTxOValue outputUTxOValue =
  let innerInputValue :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger))))
      innerInputValue = pto (pto $ pfromData inputUTxOValue)
      innerOutputValue :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger))))
      innerOutputValue = pto (pto $ pfromData outputUTxOValue)

      goOuter
        :: Term
             _
             ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger)))
                 :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger)))
                 :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
                 :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
             )
      goOuter = pfix #$ plam $ \self inputValuePairs outputValuePairs diffAccumulator ->
        pelimList
          ( \inputValueEntry inputValueEntries ->
              plet (pfstBuiltin # inputValueEntry) $ \inputValueEntryCS ->
                pelimList
                  ( \outputValueEntry outputValueEntries ->
                      pif
                        (pfromData inputValueEntryCS #== pfromData (pfstBuiltin # outputValueEntry))
                        ( pif
                            (pfromData inputValueEntryCS #== progCS)
                            ( pif
                                (pmapData # punsafeCoerce outputValueEntries #== pmapData # punsafeCoerce inputValueEntries)
                                (psubtractTokens # pto (pfromData (psndBuiltin # inputValueEntry)) # pto (pfromData @(PMap anyOrder PTokenName PInteger) (psndBuiltin # outputValueEntry)))
                                perror
                            )
                            (pif (psndBuiltin # inputValueEntry #== psndBuiltin # outputValueEntry) (self # inputValueEntries # outputValueEntries # diffAccumulator) perror)
                        )
                        (pif (psndBuiltin # inputValueEntry #== psndBuiltin # outputValueEntry) diffAccumulator perror)
                  )
                  pnil
                  outputValuePairs
          )
          pnil
          inputValuePairs
   in goOuter # innerInputValue # innerOutputValue # pnil

-- ============================================================================
-- 14. SeizeAct: processThirdPartyTransfer
-- ============================================================================
-- Plinth: processThirdPartyTransfer (with checkCorrespondingPair, accumulateProgOutputTokens)

-- | Validate one corresponding input/output pair and accumulate the delta for the seized policy.
pcheckCorrespondingThirdPartyTransferInputsAndOutputs
  :: Term s PCurrencySymbol
  -> Term s PCredential
  -> Term _ (PBuiltinList PData :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool)
  -> Term s (PBuiltinList PData)
  -> Term s (PBuiltinList (PAsData PTxInInfo))
  -> Term s (PBuiltinList (PAsData PTxOut))
  -> Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
  -> Term s PTxOut
  -> Term s PBool
pcheckCorrespondingThirdPartyTransferInputsAndOutputs programmableCS progLogicCred self remainingRelativeIdxs remainingInputsAfterIdx programmableOutputs deltaAccumulator programmableInputResolved =
  let inputTxOutConstrPair = pasConstr # pforgetData (pdata programmableInputResolved)
      correspondingOutput = phead # programmableOutputs
      outputTxOutConstrPair = pasConstr # pforgetData correspondingOutput
   in plet (psndBuiltin # inputTxOutConstrPair) $ \inputTxOutFields ->
        plet (psndBuiltin # outputTxOutConstrPair) $ \outputTxOutFields ->
          plet (phead # inputTxOutFields) $ \inputTxOutAddress ->
            plet (phead # outputTxOutFields) $ \outputTxOutAddress ->
              plet (ptail # inputTxOutFields) $ \inputTxOutFieldsRest ->
                plet (ptail # outputTxOutFields) $ \outputTxOutFieldsRest ->
                  plet (phead # (psndBuiltin # (pasConstr # inputTxOutAddress))) $ \inputCredentialData ->
                    let programmableInputValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # inputTxOutFieldsRest)
                        programmableOutputValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # outputTxOutFieldsRest)
                        programmableInputRest = ptail # inputTxOutFieldsRest
                        programmableOutputRest = ptail # outputTxOutFieldsRest
                     in pif
                          (inputCredentialData #== pforgetData (pdata progLogicCred))
                          ( pif
                              ( pand'List
                                  [ ptraceInfoIfFalse "corresponding output: address mismatch" $
                                      inputTxOutAddress #== outputTxOutAddress
                                  , ptraceInfoIfFalse "corresponding output: datum/reference script mismatch" $
                                      programmableInputRest #== programmableOutputRest
                                  ]
                              )
                              ( let delta = pvalueEqualsDeltaCurrencySymbol programmableCS programmableInputValue programmableOutputValue
                                 in self # remainingRelativeIdxs # remainingInputsAfterIdx # (ptail # programmableOutputs) # (ptokenPairsUnionFast # delta # deltaAccumulator)
                              )
                              perror
                          )
                          ( pif
                              ((pfstBuiltin # (pasConstr # inputCredentialData)) #== 1)
                              (self # remainingRelativeIdxs # remainingInputsAfterIdx # programmableOutputs # deltaAccumulator)
                              (ptraceInfoError "input index points to pubkey input")
                          )

-- | Validate the full SeizeAct mini-ledger transformation.
processThirdPartyTransfer
  :: Term s (PAsData PCurrencySymbol)
  -> Term s PCredential
  -> Term s (PBuiltinList (PAsData PTxInInfo))
  -> Term s (PBuiltinList (PAsData PTxOut))
  -> Term s (PBuiltinList PData)
  -> Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
  -> Term s PBool
processThirdPartyTransfer programmableCS progLogicCred inputs progOutputs inputIdxs' mintedTokens =
  let
    programmableCS' = pfromData programmableCS
    checkBalanceInvariant :: Term _ (PBuiltinList (PAsData PTxOut)) -> Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) -> Term _ PBool
    checkBalanceInvariant remainingOutputs deltaAccumulatorResult =
      let outputAccumulatorResult = accumulateProgOutputTokens # remainingOutputs
       in pif
            (ptokenPairsContain # outputAccumulatorResult # deltaAccumulatorResult)
            (pconstant True)
            perror

    accumulateProgOutputTokens :: Term _ (PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
    accumulateProgOutputTokens = pfix #$ plam $ \self programmableOutputs ->
      pelimList
        ( \programmableOutput programmableOutputsRest ->
            pmatch (pfromData programmableOutput) $ \(PTxOut{ptxOut'address = programmableOutputAddress, ptxOut'value = programmableOutputValue}) ->
              pif
                (paddressCredential programmableOutputAddress #== progLogicCred)
                (ptokenPairsUnionFast # (ptokensForCurrencySymbol # programmableCS' # pfromData programmableOutputValue) # (self # programmableOutputsRest))
                (self # programmableOutputsRest)
        )
        pnil
        programmableOutputs

    go :: Term _ (PBuiltinList PData :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool)
    go = pfix #$ plam $ \self relativeInputIdxs remainingInputs programmableOutputs deltaAccumulator ->
      pelimList
        ( \relativeIdxData remainingRelativeIdxs ->
            let relativeIdx = pasInt # relativeIdxData
             in plet (pdropFast # relativeIdx # remainingInputs) $ \remainingInputsAtIdx ->
                  plet (phead # remainingInputsAtIdx) $ \programmableInput ->
                    let remainingInputsAfterIdx = ptail # remainingInputsAtIdx
                     in plet (ptxInInfoResolved $ pfromData programmableInput) $ \programmableInputResolved ->
                          pcheckCorrespondingThirdPartyTransferInputsAndOutputs
                            programmableCS'
                            progLogicCred
                            self
                            remainingRelativeIdxs
                            remainingInputsAfterIdx
                            programmableOutputs
                            deltaAccumulator
                            programmableInputResolved
        )
        (checkBalanceInvariant programmableOutputs (ptokenPairsUnionFast # deltaAccumulator # mintedTokens))
        relativeInputIdxs
   in
    go # inputIdxs' # inputs # progOutputs # pnil

-- ============================================================================
-- 15. Redeemer map helpers
-- ============================================================================
-- Plinth: enforceNSpendRedeemers, isSpendingPurpose

penforceNSpendRedeemers :: Term s PInteger -> Term s (PMap 'Unsorted PScriptPurpose PRedeemer) -> Term s PBool
penforceNSpendRedeemers n rdmrs =
  let isNonSpend :: Term _ (PAsData PScriptPurpose) -> Term _ PBool
      isNonSpend red = pnot # (pfstBuiltin # (pasConstr # pforgetData red) #== 1)
      isLastSpend :: Term _ (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) :--> PBool)
      isLastSpend = plam $ \redeemers ->
        let constrPair = pfstBuiltin # (phead # redeemers)
            constrIdx = pfstBuiltin # (pasConstr # pforgetData constrPair)
         in pif
              (constrIdx #== 1)
              (pelimList (\x _ -> isNonSpend (pfstBuiltin # x)) (pconstant True) (ptail # redeemers))
              perror
   in isLastSpend # (pdropFast # (n - 1) # pto rdmrs)

-- ============================================================================
-- 16. Base validator (mkProgrammableLogicBase)
-- ============================================================================
-- Plinth: mkProgrammableLogicBaseValidator

mkProgrammableLogicBase :: Term s (PAsData PCredential :--> PScriptContext :--> PUnit)
mkProgrammableLogicBase = plam $ \stakeCred ctx ->
  pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
    let wdrls :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))
        wdrls = pto $ pfromData $ ptxInfo'wdrl txInfo
     in plet wdrls $ \withdrawals ->
          let firstWithdrawal :: Term _ (PAsData PCredential)
              firstWithdrawal = pfstBuiltin # (phead @PBuiltinList # withdrawals)
              hasCred =
                (firstWithdrawal #== stakeCred)
                  #|| let go = pfix #$ plam $ \self withdrawals' ->
                            let withdrawal = phead # withdrawals'
                             in (pfstBuiltin # withdrawal)
                                  #== stakeCred
                                  #|| plet
                                    (ptail # withdrawals')
                                    ( \withdrawals'' ->
                                        let withdrawalA = phead # withdrawals''
                                         in (pfstBuiltin # withdrawalA) #== stakeCred #|| self # (ptail # withdrawals'')
                                    )
                       in go # (ptail # withdrawals)
           in pvalidateConditions [ptraceInfoIfFalse "programmable global not invoked" hasCred]

-- ============================================================================
-- 17. Global validator (mkProgrammableLogicGlobal)
-- ============================================================================
-- Plinth: mkProgrammableLogicGlobalValidator

mkProgrammableLogicGlobal :: Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mkProgrammableLogicGlobal = plam $ \protocolParamsCS ctx -> P.do
  PScriptContext{pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'signatories, ptxInfo'wdrl, ptxInfo'mint, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
  let red = pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalRedeemer) (pto pscriptContext'redeemer)
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs

  ptraceInfo "Extracting protocol parameter UTxO"

  PProgrammableLogicGlobalParams{pdirectoryNodeCS, pprogLogicCred} <-
    pmatch $
      pfindReferenceInputByCS (pfromData protocolParamsCS) referenceInputs
  progLogicCred <- plet $ pfromData pprogLogicCred

  ptraceInfo "Extracting invoked scripts"
  withdrawalEntries <- plet $ pto (pfromData ptxInfo'wdrl)

  pmatch red $ \case
    PTransferAct transferProofs mintProofs -> P.do
      cachedTransferScript0 <- plet $ pfstBuiltin # (phead @PBuiltinList # withdrawalEntries)
      totalProgTokenValue <-
        plet $
          pvalueFromCred
            progLogicCred
            (pfromData ptxInfo'signatories)
            withdrawalEntries
            (pfromData ptxInfo'inputs)
      totalProgTokenValue_ <-
        plet $
          pcheckTransferLogicAndGetProgrammableValue
            (pfromData pdirectoryNodeCS)
            referenceInputs
            (pfromData transferProofs)
            withdrawalEntries
            cachedTransferScript0
            totalProgTokenValue
      mintValueNoGuarantees <- plet $ punsafeCoerce @(PValue 'Sorted 'NoGuarantees) (pfromData ptxInfo'mint)
      expectedProgrammableOutputValue <-
        plet $
          pif
            (pnull # pto (pto mintValueNoGuarantees))
            totalProgTokenValue_
            ( punsafeCoerce @(PValue 'Sorted 'Positive) $
                punsafeCoerce @(PValue 'Sorted 'NoGuarantees) totalProgTokenValue_
                  #<> pcheckMintLogicAndGetProgrammableValue
                    (pfromData pdirectoryNodeCS)
                    referenceInputs
                    (pfromData mintProofs)
                    withdrawalEntries
                    mintValueNoGuarantees
            )

      pvalidateConditions
        [ pisRewardingScript (pdata pscriptContext'scriptInfo)
        , ptraceInfoIfFalse "prog tokens escape" $
            poutputsContainExpectedValueAtCred
              progLogicCred
              (pfromData ptxInfo'outputs)
              expectedProgrammableOutputValue
        ]
    PSeizeAct{pdirectoryNodeIdx, pinputIdxs, poutputsStartIdx, plengthInputIdxs} -> P.do
      inputIdxsLen <- plet $ pfromData plengthInputIdxs
      let inputIdxsData = punsafeCoerce (pfromData pinputIdxs) :: Term _ (PBuiltinList PData)
      let remainingOutputs = pdropFast # pfromData poutputsStartIdx # pfromData ptxInfo'outputs
      let directoryNodeUTxO = phead # (pdropFast # pfromData pdirectoryNodeIdx # referenceInputs)
      PTxOut{ptxOut'value = seizeDirectoryNodeValue, ptxOut'datum = seizeDirectoryNodeDatum} <- pmatch (ptxInInfoResolved $ pfromData directoryNodeUTxO)
      POutputDatum seizeDat' <- pmatch seizeDirectoryNodeDatum
      PDirectorySetNode
        { pkey = directoryNodeDatumFKey
        , pissuerLogicScript = directoryNodeDatumFIssuerLogicScript
        } <-
        pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto seizeDat'))
      mintValueNoGuarantees <- plet $ punsafeCoerce @(PValue 'Sorted 'NoGuarantees) (pfromData ptxInfo'mint)
      seizeMintedTokens <- plet $ ptokensForCurrencySymbol # pfromData directoryNodeDatumFKey # mintValueNoGuarantees
      let conditions =
            [ ptraceInfoIfFalse "mini-ledger invariants violated" $ processThirdPartyTransfer directoryNodeDatumFKey progLogicCred (pfromData ptxInfo'inputs) remainingOutputs inputIdxsData seizeMintedTokens
            , ptraceInfoIfFalse "issuer logic script must be invoked" $ pisScriptInvokedEntries # directoryNodeDatumFIssuerLogicScript # withdrawalEntries
            , ptraceInfoIfFalse "directory node is not valid" $ phasCSH (pfromData pdirectoryNodeCS) seizeDirectoryNodeValue
            , ptraceInfoIfFalse "spending redeemer count mismatch" $ penforceNSpendRedeemers inputIdxsLen (pfromData ptxInfo'redeemers)
            , ptraceInfoIfFalse "input index length mismatch" $ (pbuiltinListLengthFast # inputIdxsLen # inputIdxsData) #== inputIdxsLen
            ]
      pvalidateConditions conditions
