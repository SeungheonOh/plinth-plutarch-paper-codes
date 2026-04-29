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
  mkProgrammableLogicBase,
  mkProgrammableLogicGlobal,
  isScriptInvokedEntries,
  valueFromCred,
  valueToCred,
  outputsContainExpectedValueAtCred,
  pscriptContextTxInfo,
) where

import Data.Kind (Type)
import Data.List (foldl')
import Plutarch.Builtin.Integer (pconstantInteger)
import Plutarch.Internal.Term (punsafeBuiltin)
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore qualified as PLC
import PlutusLedgerApi.V1.Value (Value)
import SmartTokens.Types.PTokenDirectory (PDirectorySetNode (..))
import SmartTokens.Types.ProgrammableLogicGlobal (PProgrammableLogicGlobalRedeemer (..))
import SmartTokens.Types.ProtocolParams (PProgrammableLogicGlobalParams (..))

-- ============================================================================
-- 1. Plutarch infrastructure
-- ============================================================================

type PType = S -> Type
type ClosedTerm (a :: PType) = forall (s :: S). Term s a

mapData :: Term s (PBuiltinList (PBuiltinPair PData PData) :--> PData)
mapData = punsafeBuiltin PLC.MapData

pairDataBuiltinRaw :: Term s (PData :--> PData :--> PBuiltinPair PData PData)
pairDataBuiltinRaw = punsafeBuiltin PLC.MkPairData

-- | Unsafely unwrap a PMaybeData known to be Just.
justData :: Term s (PMaybeData a) -> Term s a
justData term =
  punsafeCoerce $ phead # (psndBuiltin # (pasConstr # pforgetData (pdata term)))

and'List :: [Term s PBool] -> Term s PBool
and'List ts' = case ts' of
  [] -> pconstant True
  ts -> foldl1 (\res x -> pand' # res # x) ts

validateConditions :: [Term s PBool] -> Term s PUnit
validateConditions conds = pif (and'List conds) (pconstant ()) perror

-- ============================================================================
-- 2. List operations
-- ============================================================================

nTails :: (PIsListLike list a) => Integer -> Term s (list a) -> Term s (list a)
nTails n xs = foldl' (\acc _ -> ptail # acc) xs (replicate (fromIntegral n) ())

tails10 :: (PIsListLike list a) => ClosedTerm (list a :--> list a)
tails10 = phoistAcyclic $ plam (nTails 10)

tails20 :: (PIsListLike list a) => ClosedTerm (list a :--> list a)
tails20 = phoistAcyclic $ plam (\xs -> tails10 # (tails10 # xs))

tails30 :: (PIsListLike list a) => ClosedTerm (list a :--> list a)
tails30 = phoistAcyclic $ plam (\xs -> tails20 # (tails10 # xs))

dropR :: forall (list :: PType -> PType) (a :: PType) (s :: S). (PIsListLike list a) => Term s (PInteger :--> list a :--> list a)
dropR = phoistAcyclic $ pfix #$ plam $ \self n ys -> pif (n #== 0) ys (self # (n - 1) # (ptail # ys))

dropFast :: (PIsListLike PBuiltinList a) => Term s (PInteger :--> PBuiltinList a :--> PBuiltinList a)
dropFast = phoistAcyclic $
  pfix #$ plam $ \self n ys ->
    pcond
      [ (30 #<= n, self # (n - 30) # (tails30 # ys))
      , (20 #<= n, self # (n - 20) # (tails20 # ys))
      , (10 #<= n, self # (n - 10) # (tails10 # ys))
      ]
      (dropR # n # ys)

builtinListLength :: forall s a. (PElemConstraint PBuiltinList a) => Term s PInteger -> Term s (PBuiltinList a :--> PInteger)
builtinListLength acc =
  ( pfix #$ plam $ \self acc' l ->
      pelimList (\_ ys -> self # (acc' + 1) # ys) acc' l
  )
    # acc

builtinListLengthFast :: forall (a :: PType) (s :: S). (PElemConstraint PBuiltinList a) => Term s (PInteger :--> PBuiltinList a :--> PInteger)
builtinListLengthFast = phoistAcyclic $ plam $ \n elems ->
  let go :: Term _ (PInteger :--> PInteger :--> PBuiltinList a :--> PInteger)
      go = pfix #$ plam $ \self remainingExpected currentCount xs ->
        pcond
          [ (30 #<= remainingExpected, self # (remainingExpected - 30) # (currentCount + 30) # (tails30 # xs))
          , (20 #<= remainingExpected, self # (remainingExpected - 20) # (currentCount + 20) # (tails20 # xs))
          , (10 #<= remainingExpected, self # (remainingExpected - 10) # (currentCount + 10) # (tails10 # xs))
          ]
          (currentCount + builtinListLength 0 # xs)
   in go # n # 0 # elems

-- ============================================================================
-- 3. Value helpers
-- ============================================================================

emptyValue :: Value
emptyValue = mempty

emptyLedgerValue :: Term s (PValue 'Sorted 'Positive)
emptyLedgerValue = punsafeCoerce $ pconstant @(PValue 'Unsorted 'NoGuarantees) emptyValue

-- | Strip Ada (first entry) from a ledger-provided sorted value.
stripAda
  :: forall (v :: AmountGuarantees) (s :: S)
   . Term s (PValue 'Sorted v) -> Term s (PValue 'Sorted v)
stripAda value =
  let nonAdaValueMapInner = ptail # pto (pto value)
   in pcon (PValue $ pcon $ PMap nonAdaValueMapInner)

-- | Merge two sorted token-name maps by asset-wise addition.
tokenPairsUnion
  :: Term
       s
       ( PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
       )
tokenPairsUnion = phoistAcyclic $
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
currencyPairsUnion
  :: Term
       s
       ( PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
           :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
           :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger)))
       )
currencyPairsUnion = phoistAcyclic $
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
                            mergedTokenPairs = tokenPairsUnion # tokenPairsA # tokenPairsB
                            mergedPair =
                              punsafeCoerce $
                                pairDataBuiltinRaw
                                  # pforgetData currencySymbolA
                                  # (mapData # punsafeCoerce mergedTokenPairs)
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
valueUnion :: Term s (PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive)
valueUnion = phoistAcyclic $ plam $ \valueA valueB ->
  pcon $
    PValue $
      pcon $
        PMap $
          currencyPairsUnion
            # pto (pto valueA)
            # pto (pto valueB)

-- | Get the quantity of a specific asset in a value.
assetQtyInValue
  :: Term
       s
       ( PValue 'Sorted 'Positive
           :--> PCurrencySymbol
           :--> PTokenName
           :--> PInteger
       )
assetQtyInValue = phoistAcyclic $ plam $ \value cs tn ->
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
tokensForCurrencySymbol
  :: forall anyAmount s
   . Term s (PCurrencySymbol :--> PValue 'Sorted anyAmount :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
tokensForCurrencySymbol =
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
negateTokens :: Term _ (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
negateTokens = pfix #$ plam $ \self tokens ->
  pelimList
    ( \tokenPair tokensRest ->
        let tokenName = pfstBuiltin # tokenPair
            tokenAmount = psndBuiltin # tokenPair
         in pcons # (ppairDataBuiltin # tokenName # pdata (pconstantInteger 0 - pfromData tokenAmount)) # (self # tokensRest)
    )
    pnil
    tokens

-- | Subtract token pairs: input - output, skipping zero results.
subtractTokens
  :: Term
       s
       ( PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
       )
subtractTokens = phoistAcyclic $
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
      (negateTokens # outputTokens)
      inputTokens

-- | Check that actualTokens contain at least requiredTokens quantities.
tokenPairsContain
  :: Term
       s
       ( PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
           :--> PBool
       )
tokenPairsContain = phoistAcyclic $
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

getPaymentCredential :: Term s PAddress -> Term s PCredential
getPaymentCredential addr = pmatch addr $ \addr' -> paddress'credential addr'

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

-- | Check whether a credential appears in withdrawals.
isScriptInvokedEntries :: Term s (PAsData PCredential :--> PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)) :--> PBool)
isScriptInvokedEntries = phoistAcyclic $ plam $ \scriptCredData withdrawalEntries ->
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
isRewardingScriptInfo :: Term s (PAsData PScriptInfo) -> Term s PBool
isRewardingScriptInfo term = (pfstBuiltin # (pasConstr # pforgetData term)) #== 2

-- | Check if tx is signed by a pubkey hash.
txSignedByPkh :: Term s (PAsData PPubKeyHash :--> PBuiltinList (PAsData PPubKeyHash) :--> PBool)
txSignedByPkh = pelem

-- ============================================================================
-- 6. Value CS checking
-- ============================================================================

-- | Check that the first non-Ada policy matches a state-token CS.
hasCsFirstNonAda :: Term s PCurrencySymbol -> Term s (PAsData (PValue 'Sorted 'Positive)) -> Term s PBool
hasCsFirstNonAda directoryNodeCS value =
  let value' = pto (pto (pfromData value))
   in pfromData (pfstBuiltin # (phead # (ptail # value'))) #== directoryNodeCS

-- | Safe variant that returns False instead of crashing on missing non-Ada entries.
hasCsFirstNonAdaOrFalse :: Term s PCurrencySymbol -> Term s (PAsData (PValue 'Sorted 'Positive)) -> Term s PBool
hasCsFirstNonAdaOrFalse directoryNodeCS value =
  let nonAdaEntries = ptail # pto (pto (pfromData value))
   in pelimList
        (\currencyPair _ -> pfromData (pfstBuiltin # currencyPair) #== directoryNodeCS)
        (pcon PFalse)
        nonAdaEntries

-- ============================================================================
-- 7. Value aggregation
-- ============================================================================

-- | Aggregate all non-Ada input value controlled by a payment credential.
valueFromCred
  :: Term s PCredential
  -> Term s (PBuiltinList (PAsData PPubKeyHash))
  -> Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))
  -> Term s (PBuiltinList (PAsData PTxInInfo))
  -> Term s (PValue 'Sorted 'Positive)
valueFromCred cred sigs withdrawalEntries inputs =
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
                            ( pmatch (justData stakingCredMaybe) $ \case
                                PStakingHash ownerCred ->
                                  pmatch ownerCred $ \case
                                    PPubKeyCredential pkh ->
                                      pif
                                        (txSignedByPkh # pkh # sigs)
                                        (valueUnion # acc # stripAda (pfromData resolvedOutValue))
                                        (ptraceInfoError "Missing required pk witness")
                                    PScriptCredential scriptHash_ ->
                                      let scriptCredData = pdata $ pcon (PScriptCredential scriptHash_)
                                       in pif
                                            (isScriptInvokedEntries # scriptCredData # withdrawalEntries)
                                            (valueUnion # acc # stripAda (pfromData resolvedOutValue))
                                            (ptraceInfoError "Missing required script witness")
                                _ -> perror
                            )
                            acc
                          # xs
            )
            acc
      )
        # emptyLedgerValue
        # inputs

-- | Aggregate all non-Ada output value at a payment credential.
valueToCred
  :: Term s PCredential
  -> Term s (PBuiltinList (PAsData PTxOut))
  -> Term s (PValue 'Sorted 'Positive)
valueToCred cred inputs =
  let credData = pforgetData (pdata cred)
   in ( pfix #$ plam $ \self acc ->
          pelimList
            ( \txOut xs ->
                plet (psndBuiltin # (pasConstr # pforgetData txOut)) $ \txOutFields ->
                  let txOutAddress = phead # txOutFields
                      txOutFieldsRest = ptail # txOutFields
                      ptxOutValue = punsafeCoerce @(PAsData (PValue 'Sorted 'Positive)) (phead # txOutFieldsRest)
                      paymentCredData = phead # (psndBuiltin # (pasConstr # txOutAddress))
                   in self
                        # pif (paymentCredData #== credData) (valueUnion # acc # stripAda (pfromData ptxOutValue)) acc
                        # xs
            )
            acc
      )
        # emptyLedgerValue
        # inputs

-- ============================================================================
-- 8. Output containment check
-- ============================================================================

outputsContainExpectedValueAtCred
  :: Term s PCredential
  -> Term s (PBuiltinList (PAsData PTxOut))
  -> Term s (PValue 'Sorted 'Positive)
  -> Term s PBool
outputsContainExpectedValueAtCred progLogicCred txOutputs expectedValue =
  let
    hasAtLeastAssetInProgOutputs = pfix #$ plam $ \self requiredQty currentQty cs tn remainingOutputs ->
      pif
        (currentQty #>= requiredQty)
        (pconstant True)
        ( pelimList
            ( \txOut outputsRest ->
                pmatch (pfromData txOut) $ \(PTxOut{ptxOut'address, ptxOut'value}) ->
                  pif
                    (getPaymentCredential ptxOut'address #== progLogicCred)
                    (self # requiredQty # (currentQty + (assetQtyInValue # pfromData ptxOut'value # cs # tn)) # cs # tn # outputsRest)
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
             in (assetQtyInValue # actualValue # expectedCurrencySymbol # expectedTokenName #>= expectedTokenQty)
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
    actualValueAtCred = valueToCred progLogicCred txOutputs
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

findReferenceInputByCS
  :: Term s PCurrencySymbol
  -> Term s (PBuiltinList (PAsData PTxInInfo))
  -> Term s PProgrammableLogicGlobalParams
findReferenceInputByCS currencySymbol referenceInputs =
  let extractParams resolvedOut =
        pmatch (ptxOutDatum resolvedOut) $ \case
          POutputDatum paramDat' ->
            pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalParams) (pto paramDat')
          _ -> ptraceInfoError "protocol params datum missing"
      go = pfix #$ plam $ \self remainingRefInputs ->
        let txIn = phead # remainingRefInputs
         in plet (ptxInInfoResolved $ pfromData txIn) $ \resolvedOut ->
              pif
                (hasCsFirstNonAdaOrFalse currencySymbol (ptxOutValue resolvedOut))
                (extractParams resolvedOut)
                (self # (ptail # remainingRefInputs))
   in go # referenceInputs

-- ============================================================================
-- 10. Transfer logic validation
-- ============================================================================

checkTransferLogicAndGetProgrammableValue
  :: Term s PCurrencySymbol
  -> Term s (PBuiltinList (PAsData PTxInInfo))
  -> Term s (PBuiltinList (PAsData PInteger))
  -> Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))
  -> Term s (PAsData PCredential)
  -> Term s (PValue 'Sorted 'Positive)
  -> Term s (PValue 'Sorted 'Positive)
checkTransferLogicAndGetProgrammableValue directoryNodeCS refInputs proofList withdrawalEntries initialCachedTransferScript totalValue =
  let mapInnerList :: Term _ (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap 'Sorted PTokenName PInteger))))
      mapInnerList = pto (pto totalValue)
      go = pfix #$ plam $ \self proofs inputInnerValue actualProgrammableTokenValue cachedTransferScript ->
        pelimList
          ( \csPair csPairs ->
              P.do
                PTxOut{ptxOut'value = directoryNodeUTxOFValue, ptxOut'datum = directoryNodeUTxOFDatum} <-
                  pmatch $ ptxInInfoResolved (pfromData $ phead # (dropFast # pfromData (phead # proofs) # refInputs))
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
                          and'List
                            [ ptraceInfoIfFalse "dir neg-proof node must cover" (currCS #< nodeNext)
                            , ptraceInfoIfFalse "invalid dir node n" (hasCsFirstNonAda directoryNodeCS directoryNodeUTxOFValue)
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
                          and'List
                            [ ptraceInfoIfFalse "Missing required transfer script" $
                                (directoryNodeDatumFTransferLogicScript #== cachedTransferScript)
                                  #|| (isScriptInvokedEntries # directoryNodeDatumFTransferLogicScript # withdrawalEntries)
                            , ptraceInfoIfFalse "directory proof mismatch" (nodeKey #== currCS)
                            , ptraceInfoIfFalse "invalid dir node" (hasCsFirstNonAda directoryNodeCS directoryNodeUTxOFValue)
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
        # pto (pto emptyLedgerValue)
        # initialCachedTransferScript

-- ============================================================================
-- 11. Mint logic validation
-- ============================================================================

checkMintLogicAndGetProgrammableValue
  :: Term s PCurrencySymbol
  -> Term s (PBuiltinList (PAsData PTxInInfo))
  -> Term s (PBuiltinList (PAsData PInteger))
  -> Term s (PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)))
  -> Term s (PValue 'Sorted 'NoGuarantees)
  -> Term s (PValue 'Sorted 'NoGuarantees)
checkMintLogicAndGetProgrammableValue directoryNodeCS refInputs proofList withdrawalEntries totalMintValue =
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
                            pmatch $ ptxInInfoResolved (pfromData $ phead # (dropFast # pfromData nodeIdx # refInputs))
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
                                    and'List
                                      [ ptraceInfoIfFalse "Missing required transfer script" (isScriptInvokedEntries # directoryNodeDatumFTransferLogicScript # withdrawalEntries)
                                      , ptraceInfoIfFalse "invalid dir node m" (hasCsFirstNonAda directoryNodeCS directoryNodeUTxOFValue)
                                      ]
                               in pif
                                    checks
                                    (self # proofsRest # mintCsPairs # (pcons # mintCsPair # programmableMintValue))
                                    perror
                            )
                            ( let checks =
                                    and'List
                                      [ ptraceInfoIfFalse "dir mint neg-proof node must cover" (nodeKey #< currCS)
                                      , ptraceInfoIfFalse "dir mint neg-proof node must cover" (currCS #< nodeNext)
                                      , ptraceInfoIfFalse "invalid dir node n" (hasCsFirstNonAda directoryNodeCS directoryNodeUTxOFValue)
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
-- 12. SeizeAct: value delta
-- ============================================================================

-- | Compare two values; require equality everywhere except one CS; return that CS's signed delta.
valueEqualsDeltaCurrencySymbol
  :: forall anyOrder anyAmount s
   . Term s PCurrencySymbol
  -> Term s (PAsData (PValue anyOrder anyAmount))
  -> Term s (PAsData (PValue anyOrder anyAmount))
  -> Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
valueEqualsDeltaCurrencySymbol progCS inputUTxOValue outputUTxOValue =
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
                                (mapData # punsafeCoerce outputValueEntries #== mapData # punsafeCoerce inputValueEntries)
                                (subtractTokens # pto (pfromData (psndBuiltin # inputValueEntry)) # pto (pfromData @(PMap anyOrder PTokenName PInteger) (psndBuiltin # outputValueEntry)))
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

-- | Validate one corresponding input/output pair and accumulate the delta for the seized policy.
checkCorrespondingThirdPartyTransferInputsAndOutputs
  :: Term s PCurrencySymbol
  -> Term s PCredential
  -> Term _ (PBuiltinList PData :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool)
  -> Term s (PBuiltinList PData)
  -> Term s (PBuiltinList (PAsData PTxInInfo))
  -> Term s (PBuiltinList (PAsData PTxOut))
  -> Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
  -> Term s PTxOut
  -> Term s PBool
checkCorrespondingThirdPartyTransferInputsAndOutputs programmableCS progLogicCred self remainingRelativeIdxs remainingInputsAfterIdx programmableOutputs deltaAccumulator programmableInputResolved =
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
                              ( and'List
                                  [ ptraceInfoIfFalse "corresponding output: address mismatch" $
                                      inputTxOutAddress #== outputTxOutAddress
                                  , ptraceInfoIfFalse "corresponding output: datum/reference script mismatch" $
                                      programmableInputRest #== programmableOutputRest
                                  ]
                              )
                              ( let delta = valueEqualsDeltaCurrencySymbol programmableCS programmableInputValue programmableOutputValue
                                 in self # remainingRelativeIdxs # remainingInputsAfterIdx # (ptail # programmableOutputs) # (tokenPairsUnion # delta # deltaAccumulator)
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
            (tokenPairsContain # outputAccumulatorResult # deltaAccumulatorResult)
            (pconstant True)
            perror

    accumulateProgOutputTokens :: Term _ (PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
    accumulateProgOutputTokens = pfix #$ plam $ \self programmableOutputs ->
      pelimList
        ( \programmableOutput programmableOutputsRest ->
            pmatch (pfromData programmableOutput) $ \(PTxOut{ptxOut'address = programmableOutputAddress, ptxOut'value = programmableOutputValue}) ->
              pif
                (getPaymentCredential programmableOutputAddress #== progLogicCred)
                (tokenPairsUnion # (tokensForCurrencySymbol # programmableCS' # pfromData programmableOutputValue) # (self # programmableOutputsRest))
                (self # programmableOutputsRest)
        )
        pnil
        programmableOutputs

    go :: Term _ (PBuiltinList PData :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTxOut) :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) :--> PBool)
    go = pfix #$ plam $ \self relativeInputIdxs remainingInputs programmableOutputs deltaAccumulator ->
      pelimList
        ( \relativeIdxData remainingRelativeIdxs ->
            let relativeIdx = pasInt # relativeIdxData
             in plet (dropFast # relativeIdx # remainingInputs) $ \remainingInputsAtIdx ->
                  plet (phead # remainingInputsAtIdx) $ \programmableInput ->
                    let remainingInputsAfterIdx = ptail # remainingInputsAtIdx
                     in plet (ptxInInfoResolved $ pfromData programmableInput) $ \programmableInputResolved ->
                          checkCorrespondingThirdPartyTransferInputsAndOutputs
                            programmableCS'
                            progLogicCred
                            self
                            remainingRelativeIdxs
                            remainingInputsAfterIdx
                            programmableOutputs
                            deltaAccumulator
                            programmableInputResolved
        )
        (checkBalanceInvariant programmableOutputs (tokenPairsUnion # deltaAccumulator # mintedTokens))
        relativeInputIdxs
   in
    go # inputIdxs' # inputs # progOutputs # pnil

-- ============================================================================
-- 15. Redeemer map helpers
-- ============================================================================

enforceNSpendRedeemers :: Term s PInteger -> Term s (PMap 'Unsorted PScriptPurpose PRedeemer) -> Term s PBool
enforceNSpendRedeemers n rdmrs =
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
   in isLastSpend # (dropFast # (n - 1) # pto rdmrs)

-- ============================================================================
-- 16. Base validator (mkProgrammableLogicBase)
-- ============================================================================

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
           in validateConditions [ptraceInfoIfFalse "programmable global not invoked" hasCred]

-- ============================================================================
-- 17. Global validator (mkProgrammableLogicGlobal)
-- ============================================================================

mkProgrammableLogicGlobal :: Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mkProgrammableLogicGlobal = plam $ \protocolParamsCS ctx -> P.do
  PScriptContext{pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'signatories, ptxInfo'wdrl, ptxInfo'mint, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
  let red = pfromData $ punsafeCoerce @(PAsData PProgrammableLogicGlobalRedeemer) (pto pscriptContext'redeemer)
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs

  ptraceInfo "Extracting protocol parameter UTxO"

  PProgrammableLogicGlobalParams{pdirectoryNodeCS, pprogLogicCred} <-
    pmatch $
      findReferenceInputByCS (pfromData protocolParamsCS) referenceInputs
  progLogicCred <- plet $ pfromData pprogLogicCred

  ptraceInfo "Extracting invoked scripts"
  withdrawalEntries <- plet $ pto (pfromData ptxInfo'wdrl)

  pmatch red $ \case
    PTransferAct transferProofs mintProofs -> P.do
      cachedTransferScript0 <- plet $ pfstBuiltin # (phead @PBuiltinList # withdrawalEntries)
      totalProgTokenValue <-
        plet $
          valueFromCred
            progLogicCred
            (pfromData ptxInfo'signatories)
            withdrawalEntries
            (pfromData ptxInfo'inputs)
      totalProgTokenValue_ <-
        plet $
          checkTransferLogicAndGetProgrammableValue
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
                  #<> checkMintLogicAndGetProgrammableValue
                    (pfromData pdirectoryNodeCS)
                    referenceInputs
                    (pfromData mintProofs)
                    withdrawalEntries
                    mintValueNoGuarantees
            )

      validateConditions
        [ isRewardingScriptInfo (pdata pscriptContext'scriptInfo)
        , ptraceInfoIfFalse "prog tokens escape" $
            outputsContainExpectedValueAtCred
              progLogicCred
              (pfromData ptxInfo'outputs)
              expectedProgrammableOutputValue
        ]
    PSeizeAct{pdirectoryNodeIdx, pinputIdxs, poutputsStartIdx, plengthInputIdxs} -> P.do
      inputIdxsLen <- plet $ pfromData plengthInputIdxs
      let inputIdxsData = punsafeCoerce (pfromData pinputIdxs) :: Term _ (PBuiltinList PData)
      let remainingOutputs = dropFast # pfromData poutputsStartIdx # pfromData ptxInfo'outputs
      let directoryNodeUTxO = phead # (dropFast # pfromData pdirectoryNodeIdx # referenceInputs)
      PTxOut{ptxOut'value = seizeDirectoryNodeValue, ptxOut'datum = seizeDirectoryNodeDatum} <- pmatch (ptxInInfoResolved $ pfromData directoryNodeUTxO)
      POutputDatum seizeDat' <- pmatch seizeDirectoryNodeDatum
      PDirectorySetNode
        { pkey = directoryNodeDatumFKey
        , pissuerLogicScript = directoryNodeDatumFIssuerLogicScript
        } <-
        pmatch (pfromData $ punsafeCoerce @(PAsData PDirectorySetNode) (pto seizeDat'))
      mintValueNoGuarantees <- plet $ punsafeCoerce @(PValue 'Sorted 'NoGuarantees) (pfromData ptxInfo'mint)
      seizeMintedTokens <- plet $ tokensForCurrencySymbol # pfromData directoryNodeDatumFKey # mintValueNoGuarantees
      let conditions =
            [ ptraceInfoIfFalse "mini-ledger invariants violated" $ processThirdPartyTransfer directoryNodeDatumFKey progLogicCred (pfromData ptxInfo'inputs) remainingOutputs inputIdxsData seizeMintedTokens
            , ptraceInfoIfFalse "issuer logic script must be invoked" $ isScriptInvokedEntries # directoryNodeDatumFIssuerLogicScript # withdrawalEntries
            , ptraceInfoIfFalse "directory node is not valid" $ hasCsFirstNonAda (pfromData pdirectoryNodeCS) seizeDirectoryNodeValue
            , ptraceInfoIfFalse "spending redeemer count mismatch" $ enforceNSpendRedeemers inputIdxsLen (pfromData ptxInfo'redeemers)
            , ptraceInfoIfFalse "input index length mismatch" $ (builtinListLengthFast # inputIdxsLen # inputIdxsData) #== inputIdxsLen
            ]
      validateConditions conditions
