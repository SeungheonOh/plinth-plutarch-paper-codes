{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Hydra.Contracts.Head (
  mkHeadValidator,
) where

import Plutarch.Internal.Term (punsafeBuiltin)
import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Value (pcheckBinRel)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore qualified as PLC
import PlutusLedgerApi.V3 (TokenName (..))

import Hydra.Types.HeadState (
  PCloseRedeemer (..),
  PClosedDatum (..),
  PCommit (..),
  PContestRedeemer (..),
  PDecrementRedeemer (..),
  PIncrementRedeemer (..),
  PInput (..),
  POpenDatum (..),
  PState (..),
 )

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

pcheck :: Term s PBool -> Term s PUnit
pcheck b = pif b (pconstant ()) perror

plistLength :: (PIsListLike l a) => Term s (l a :--> PInteger)
plistLength = phoistAcyclic $ pfix #$ plam $ \self xs ->
  pelimList (\_ rest -> 1 + self # rest) 0 xs

plistTake :: (PIsListLike l a) => Term s (PInteger :--> l a :--> l a)
plistTake = phoistAcyclic $ pfix #$ plam $ \self n xs ->
  pif (n #<= 0) pnil (pelimList (\h t -> pcons # h # (self # (n - 1) # t)) pnil xs)

plistDrop :: (PIsListLike l a) => Term s (PInteger :--> l a :--> l a)
plistDrop = phoistAcyclic $ pfix #$ plam $ \self n xs ->
  pif (n #<= 0) xs (pelimList (\_ t -> self # (n - 1) # t) pnil xs)

plistAll :: (PIsListLike l a) => Term s ((a :--> PBool) :--> l a :--> PBool)
plistAll = phoistAcyclic $ pfix #$ plam $ \self pred' xs ->
  pelimList (\h t -> pred' # h #&& self # pred' # t) (pconstant True) xs

plistElem :: (PIsListLike l a, PEq a) => Term s (a :--> l a :--> PBool)
plistElem = phoistAcyclic $ pfix #$ plam $ \self x xs ->
  pelimList (\h t -> pif (h #== x) (pconstant True) (self # x # t)) (pconstant False) xs

plistNotElem :: (PIsListLike l a, PEq a) => Term s (a :--> l a :--> PBool)
plistNotElem = phoistAcyclic $ plam $ \x xs -> pnot # (plistElem # x # xs)

plistFind :: (PIsListLike l a) => Term s ((a :--> PBool) :--> l a :--> PMaybe a)
plistFind = phoistAcyclic $ pfix #$ plam $ \self pred' xs ->
  pelimList
    (\h t -> pif (pred' # h) (pcon (PJust h)) (self # pred' # t))
    (pcon PNothing)
    xs

pvalueGeq :: Term s (PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive :--> PBool)
pvalueGeq = phoistAcyclic $ plam $ \v1 v2 ->
  pcheckBinRel # plam (\a b -> a #>= b) # v1 # v2

pfoldMapValues :: Term s (PBuiltinList (PAsData PTxOut) :--> PValue 'Sorted 'Positive)
pfoldMapValues = phoistAcyclic $ plam $ \outs ->
  let go = pfix #$ plam $ \self xs ->
        pelimList
          ( \h t ->
              pmatch (pfromData h) $ \(PTxOut{ptxOut'value = valD}) ->
                pfromData valD <> self # t
          )
          mempty
          xs
   in go # outs

-- ============================================================================
-- 2. Hydra constants
-- ============================================================================

phydraHeadV2 :: Term s PByteString
phydraHeadV2 = pconstant "HydraHeadV2"

pverifyEd25519Signature :: Term s (PByteString :--> PByteString :--> PByteString :--> PBool)
pverifyEd25519Signature = punsafeBuiltin PLC.VerifyEd25519Signature

-- ============================================================================
-- 3. Utility functions
-- ============================================================================

phasST :: Term s (PCurrencySymbol :--> PValue 'Sorted anyAmount :--> PBool)
phasST = phoistAcyclic $ plam $ \headPolicyId v ->
  let go = pfix #$ plam $ \self entries ->
        pelimList
          ( \pair rest ->
              pif
                (pfromData (pfstBuiltin # pair) #== headPolicyId)
                ( let tokenMap = pto (pfromData (psndBuiltin # pair))
                      goTokens = pfix #$ plam $ \self' tpairs ->
                        pelimList
                          ( \tp trest ->
                              pif
                                (pfromData (pfstBuiltin # tp) #== pconstant (TokenName "HydraHeadV2"))
                                (pfromData (psndBuiltin # tp) #== 1)
                                (self' # trest)
                          )
                          (pconstant False)
                          tpairs
                   in goTokens # tokenMap
                )
                (self # rest)
          )
          (pconstant False)
          entries
   in go # pto (pto v)

pmustBurnAllHeadTokens :: Term s (PValue 'Sorted 'NoGuarantees :--> PCurrencySymbol :--> PBuiltinList (PAsData PByteString) :--> PBool)
pmustBurnAllHeadTokens = phoistAcyclic $ plam $ \mintVal headCS parties ->
  let mintMap = pto (pto mintVal)
      burntTokens =
        let go = pfix #$ plam $ \self entries ->
              pelimList
                ( \pair rest ->
                    pif
                      (pfromData (pfstBuiltin # pair) #== headCS)
                      ( let tokenMap = pto (pfromData (psndBuiltin # pair))
                            sumTokens = pfix #$ plam $ \self' tpairs ->
                              pelimList
                                (\tp trest -> pfromData (psndBuiltin # tp) + self' # trest)
                                0
                                tpairs
                         in pnegate # (sumTokens # tokenMap)
                      )
                      (self # rest)
                )
                0
                entries
         in go # mintMap
   in ptraceIfFalse "H14" (burntTokens #== plistLength # parties + 1)

pmustNotMintOrBurn :: Term s (PTxInfo :--> PBool)
pmustNotMintOrBurn = phoistAcyclic $ plam $ \txInfo ->
  pmatch txInfo $ \txI ->
    let mintVal = pto (pto (pfromData $ ptxInfo'mint txI))
     in ptraceIfFalse "U01" (pnull # mintVal)

pmustPreserveHeadValue :: Term s (PScriptContext :--> PBool)
pmustPreserveHeadValue = phoistAcyclic $ plam $ \ctx ->
  pmatch (pfindOwnInput # ctx) $ \case
    PNothing -> ptraceInfoError "H4"
    PJust ownInput ->
      pmatch ownInput $ \(PTxInInfo{ptxInInfo'resolved = resolvedIn}) ->
        pmatch resolvedIn $ \(PTxOut{ptxOut'value = inValD}) ->
          pmatch ctx $ \(PScriptContext{pscriptContext'txInfo}) ->
            pmatch pscriptContext'txInfo $ \txI ->
              let outputs = pfromData $ ptxInfo'outputs txI
                  firstOut = phead # outputs
               in pmatch (pfromData firstOut) $ \(PTxOut{ptxOut'value = outValD}) ->
                    let inVal = pfromData inValD
                        outVal = pfromData outValD
                     in ptraceIfFalse "H4" (pvalueGeq # outVal # inVal)

pfindOwnInput :: Term s (PScriptContext :--> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $ plam $ \ctx ->
  pmatch ctx $ \(PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo}) ->
    pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript ref _ ->
        let txInputs = pfromData $ pmatch pscriptContext'txInfo $ \txI -> ptxInfo'inputs txI
            go = pfix #$ plam $ \self inputs ->
              pelimList
                ( \inp rest ->
                    pmatch (pfromData inp) $ \(PTxInInfo{ptxInInfo'outRef}) ->
                      pif (ptxInInfo'outRef #== ref) (pcon (PJust (pfromData inp))) (self # rest)
                )
                (pcon PNothing)
                inputs
         in go # txInputs
      _ -> pcon PNothing

phashTxOuts :: Term s (PBuiltinList (PAsData PTxOut) :--> PByteString)
phashTxOuts = phoistAcyclic $ plam $ \outs ->
  let go = pfix #$ plam $ \self xs ->
        pelimList
          (\h t -> pserialiseData # pforgetData h <> self # t)
          mempty
          xs
   in psha2_256 # (go # outs)

pemptyHash :: Term s PByteString
pemptyHash = psha2_256 # pconstant ""

pcompareRef :: Term s (PTxOutRef :--> PTxOutRef :--> PInteger)
pcompareRef = phoistAcyclic $ plam $ \ref1 ref2 ->
  let fields1 = psndBuiltin # (pasConstr # pforgetData (pdata ref1))
      fields2 = psndBuiltin # (pasConstr # pforgetData (pdata ref2))
      -- TxOutRef = Constr 0 [TxId(Constr 0 [bs]), idx]
      -- TxId is Constr 0 [bytestring]
      txId1Data = phead # fields1
      txId2Data = phead # fields2
      txId1Fields = psndBuiltin # (pasConstr # txId1Data)
      txId2Fields = psndBuiltin # (pasConstr # txId2Data)
      bs1 = punsafeCoerce @PByteString (phead # txId1Fields)
      bs2 = punsafeCoerce @PByteString (phead # txId2Fields)
      idx1 = punsafeCoerce @PInteger (phead # (ptail # fields1))
      idx2 = punsafeCoerce @PInteger (phead # (ptail # fields2))
      txIdCmp =
        pif
          (bs1 #== bs2)
          0
          (pif (bs1 #< bs2) (-1) 1)
   in pif
        (txIdCmp #== 0)
        (pif (idx1 #== idx2) 0 (pif (idx1 #< idx2) (-1) 1))
        txIdCmp

phashPreSerializedCommits :: Term s (PBuiltinList (PAsData PCommit) :--> PByteString)
phashPreSerializedCommits = phoistAcyclic $ plam $ \commits ->
  let sorted = pinsertionSort # commits
      go = pfix #$ plam $ \self xs ->
        pelimList
          ( \h t ->
              pmatch (pfromData h) $ \(PCommit{pcommitPreSerializedOutput = preSerD}) ->
                pfromData preSerD <> self # t
          )
          mempty
          xs
   in psha2_256 # (go # sorted)
 where
  pinsertionSort :: Term s (PBuiltinList (PAsData PCommit) :--> PBuiltinList (PAsData PCommit))
  pinsertionSort = pfix #$ plam $ \self xs ->
    pelimList
      (\h t -> pinsert # h # (self # t))
      pnil
      xs

  pinsert :: Term s (PAsData PCommit :--> PBuiltinList (PAsData PCommit) :--> PBuiltinList (PAsData PCommit))
  pinsert = pfix #$ plam $ \self x ys ->
    pelimList
      ( \h t ->
          let xRef = pmatch (pfromData x) $ \(PCommit{pcommitInput = refD}) -> pfromData refD
              hRef = pmatch (pfromData h) $ \(PCommit{pcommitInput = refD}) -> pfromData refD
              cmp = pcompareRef # xRef # hRef
           in pif
                (cmp #<= 0)
                (pcons # x # (pcons # h # t))
                (pcons # h # (self # x # t))
      )
      (pcons # x # pnil)
      ys

-- ============================================================================
-- 4. ContestationPeriod helpers
-- ============================================================================

paddContestationPeriod :: Term s (PInteger :--> PInteger :--> PInteger)
paddContestationPeriod = phoistAcyclic $ plam $ \time ms -> time + ms

-- ============================================================================
-- 5. DepositDatum
-- ============================================================================

pdepositDatum :: Term s (PTxOut :--> PBuiltinList (PAsData PCommit))
pdepositDatum = phoistAcyclic $ plam $ \txOut ->
  pmatch txOut $ \(PTxOut{ptxOut'datum}) ->
    pmatch ptxOut'datum $ \case
      POutputDatum d ->
        let datumData = pto d
            constrPair = pasConstr # datumData
            fields = psndBuiltin # constrPair
            commitsData = phead # (ptail # (ptail # fields))
         in punsafeCoerce @(PBuiltinList (PAsData PCommit)) (pasList # commitsData)
      _ -> pnil

-- ============================================================================
-- 6. Head validator helpers
-- ============================================================================

pgetHeadInput :: Term s (PScriptContext :--> PTxInInfo)
pgetHeadInput = phoistAcyclic $ plam $ \ctx ->
  pmatch (pfindOwnInput # ctx) $ \case
    PJust x -> x
    PNothing -> ptraceInfoError "H8"

pgetHeadAddress :: Term s (PScriptContext :--> PAddress)
pgetHeadAddress = phoistAcyclic $ plam $ \ctx ->
  pmatch (pfindOwnInput # ctx) $ \case
    PJust ownInput -> pmatch ownInput $ \(PTxInInfo{ptxInInfo'resolved}) ->
      pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'address}) -> ptxOut'address
    PNothing -> ptraceInfoError "H8"

pmustNotChangeParameters
  :: Term
       s
       ( PBuiltinList (PAsData PByteString)
           :--> PBuiltinList (PAsData PByteString)
           :--> PInteger
           :--> PInteger
           :--> PCurrencySymbol
           :--> PCurrencySymbol
           :--> PBool
       )
pmustNotChangeParameters = phoistAcyclic $ plam $ \prevParties nextParties prevCperiod nextCperiod prevHeadId nextHeadId ->
  ptraceIfFalse "H2" $
    pdata prevParties
      #== pdata nextParties
      #&& prevCperiod
      #== nextCperiod
      #&& prevHeadId
      #== nextHeadId

pmakeContestationDeadline :: Term s (PInteger :--> PScriptContext :--> PInteger)
pmakeContestationDeadline = phoistAcyclic $ plam $ \cperiod ctx ->
  pmatch ctx $ \(PScriptContext{pscriptContext'txInfo}) ->
    pmatch pscriptContext'txInfo $ \txI ->
      pmatch (ptxInfo'validRange txI) $ \(PInterval _ ub) ->
        pmatch ub $ \(PUpperBound ext _) ->
          pmatch ext $ \case
            PFinite t -> pfromData (punsafeCoerce @(PAsData PInteger) t) + cperiod
            _ -> ptraceInfoError "H27"

pheadOutputDatum :: Term s (PScriptContext :--> PDatum)
pheadOutputDatum = phoistAcyclic $ plam $ \ctx ->
  let headAddr = pgetHeadAddress # ctx
   in pmatch ctx $ \(PScriptContext{pscriptContext'txInfo}) ->
        pmatch pscriptContext'txInfo $ \txI ->
          let outputs = pfromData $ ptxInfo'outputs txI
              firstOut = phead # outputs
           in pmatch (pfromData firstOut) $ \(PTxOut{ptxOut'address, ptxOut'datum}) ->
                pif
                  (ptxOut'address #== headAddr)
                  ( pmatch ptxOut'datum $ \case
                      POutputDatum d -> d
                      _ -> ptraceInfoError "H9"
                  )
                  (ptraceInfoError "H11")

pdecodeHeadOutputClosedDatum :: Term s (PScriptContext :--> PClosedDatum)
pdecodeHeadOutputClosedDatum = phoistAcyclic $ plam $ \ctx ->
  let datum = pheadOutputDatum # ctx
   in pmatch (pfromData $ punsafeCoerce @(PAsData PState) (pto datum)) $ \case
        PClosed closedAsData -> pfromData closedAsData
        _ -> ptraceInfoError "H3"

pdecodeHeadOutputOpenDatum :: Term s (PScriptContext :--> POpenDatum)
pdecodeHeadOutputOpenDatum = phoistAcyclic $ plam $ \ctx ->
  let datum = pheadOutputDatum # ctx
   in pmatch (pfromData $ punsafeCoerce @(PAsData PState) (pto datum)) $ \case
        POpen openAsData -> pfromData openAsData
        _ -> ptraceInfoError "H3"

pfindParticipationTokens :: Term s (PCurrencySymbol :--> PValue 'Sorted anyAmount :--> PBuiltinList (PAsData PTokenName))
pfindParticipationTokens = phoistAcyclic $ plam $ \headCS v ->
  let entries = pto (pto v)
      go = pfix #$ plam $ \self pairs ->
        pelimList
          ( \pair rest ->
              pif
                (pfromData (pfstBuiltin # pair) #== headCS)
                ( let tokenMap = pto (pfromData (psndBuiltin # pair))
                      filterPTs = pfix #$ plam $ \self' tpairs ->
                        pelimList
                          ( \tp trest ->
                              let tn = pfstBuiltin # tp
                                  qty = pfromData (psndBuiltin # tp)
                               in pif
                                    (qty #== 1 #&& pnot # (pfromData tn #== pconstant (TokenName "HydraHeadV2")))
                                    (pcons # tn # (self' # trest))
                                    (self' # trest)
                          )
                          pnil
                          tpairs
                   in filterPTs # tokenMap
                )
                (self # rest)
          )
          pnil
          pairs
   in go # entries

pmustBeSignedByParticipant :: Term s (PScriptContext :--> PCurrencySymbol :--> PBool)
pmustBeSignedByParticipant = phoistAcyclic $ plam $ \ctx headCS ->
  pmatch ctx $ \(PScriptContext{pscriptContext'txInfo}) ->
    pmatch pscriptContext'txInfo $ \txI ->
      let signatories = pfromData $ ptxInfo'signatories txI
          inputs = pfromData $ ptxInfo'inputs txI
       in pelimList
            ( \signer rest ->
                pelimList
                  (\_ _ -> ptraceInfoError "H7")
                  ( let signerBytes = pmatch (pfromData signer) $ \(PPubKeyHash bbs) -> bbs
                        allPTs = pfix #$ plam $ \self inps ->
                          pelimList
                            ( \inp rest' ->
                                pmatch (pfromData inp) $ \(PTxInInfo{ptxInInfo'resolved}) ->
                                  pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'value}) ->
                                    let pts = pfindParticipationTokens # headCS # pfromData ptxOut'value
                                     in plistAppend # pts # (self # rest')
                            )
                            pnil
                            inps
                        pts = allPTs # inputs
                        ptBytes = pmap # plam (\tn -> pmatch (pfromData tn) $ \(PTokenName bs) -> bs) # pts
                     in ptraceIfFalse "H5" (plistElem # signerBytes # ptBytes)
                  )
                  rest
            )
            (ptraceInfoError "H6")
            signatories
 where
  plistAppend :: (PIsListLike l a) => Term s (l a :--> l a :--> l a)
  plistAppend = pfix #$ plam $ \self xs ys ->
    pelimList (\h t -> pcons # h # (self # t # ys)) ys xs

-- ============================================================================
-- 7. Snapshot signature verification
-- ============================================================================

pverifySnapshotSignature
  :: Term
       s
       ( PBuiltinList (PAsData PByteString)
           :--> PCurrencySymbol
           :--> PInteger
           :--> PInteger
           :--> PByteString
           :--> PByteString
           :--> PByteString
           :--> PBuiltinList (PAsData PByteString)
           :--> PBool
       )
pverifySnapshotSignature = phoistAcyclic $ plam $ \parties headId version snapshotNum utxoH commitH decommitH sigs ->
  let numParties = plistLength # parties
      numSigs = plistLength # sigs
      message =
        pserialiseData # pforgetData (pdata headId)
          <> pserialiseData # pforgetData (pdata version)
          <> pserialiseData # pforgetData (pdata snapshotNum)
          <> pserialiseData # pforgetData (pdata utxoH)
          <> pserialiseData # pforgetData (pdata commitH)
          <> pserialiseData # pforgetData (pdata decommitH)
      verifyAll = pfix #$ plam $ \self ps ss ->
        pelimList
          ( \p prest ->
              pelimList
                ( \sig srest ->
                    pverifyEd25519Signature
                      # pfromData p
                      # message
                      # pfromData sig
                      #&& self
                      # prest
                      # srest
                )
                (pconstant False)
                ss
          )
          (pconstant True)
          ps
   in ptraceIfFalse "H12" (numParties #== numSigs #&& verifyAll # parties # sigs)

-- ============================================================================
-- 8. checkIncrement
-- ============================================================================

pcheckIncrement :: Term s (PScriptContext :--> POpenDatum :--> PIncrementRedeemer :--> PUnit)
pcheckIncrement = phoistAcyclic $ plam $ \ctx openBefore redeemer -> P.do
  PScriptContext{pscriptContext'txInfo} <- pmatch ctx
  PTxInfo
    { ptxInfo'inputs
    , ptxInfo'outputs
    } <-
    pmatch pscriptContext'txInfo
  POpenDatum
    { popenParties = prevPartiesD
    , popenContestationPeriod = prevCperiodD
    , popenHeadId = prevHeadIdD
    , popenVersion = prevVersionD
    } <-
    pmatch openBefore
  PIncrementRedeemer
    { pincrementSig = sigD
    , pincrementSnapshotNumber = snD
    , pincrementRef = incRefD
    } <-
    pmatch redeemer
  nextOpenDatum <- plet $ pdecodeHeadOutputOpenDatum # ctx
  POpenDatum
    { popenUtxoHash = nextUtxoHashD
    , popenParties = nextPartiesD
    , popenContestationPeriod = nextCperiodD
    , popenHeadId = nextHeadIdD
    , popenVersion = nextVersionD
    } <-
    pmatch nextOpenDatum

  let inputs = pfromData ptxInfo'inputs
      outputs = pfromData ptxInfo'outputs
      prevHeadId = pfromData prevHeadIdD
      prevVersion = pfromData prevVersionD
      nextVersion = pfromData nextVersionD
      incRef = pfromData incRefD
      sn = pfromData snD

      -- Find deposit input by matching incrementRef against inputs
      depositInput =
        pmatch (plistFind # plam (\inp -> pmatch (pfromData inp) $ \(PTxInInfo{ptxInInfo'outRef}) -> ptxInInfo'outRef #== incRef) # inputs) $ \case
          PJust i -> pfromData i
          PNothing -> ptraceInfoError "H44"

      depositTxOut = pmatch depositInput $ \(PTxInInfo{ptxInInfo'resolved}) -> ptxInInfo'resolved
      commits = pdepositDatum # depositTxOut
      depositHash = phashPreSerializedCommits # commits
      depositValue = pmatch depositTxOut $ \(PTxOut{ptxOut'value = valD}) -> pfromData valD

      -- Find head input value (input with hasST)
      headInValue =
        pmatch (plistFind # plam (\inp -> pmatch (pfromData inp) $ \(PTxInInfo{ptxInInfo'resolved}) -> pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'value = valD}) -> phasST # prevHeadId # pfromData valD) # inputs) $ \case
          PJust i -> pmatch (pfromData i) $ \(PTxInInfo{ptxInInfo'resolved}) -> pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'value = valD}) -> pfromData valD
          PNothing -> ptraceInfoError "H45"

      -- Get head output value (first output)
      headOutValue = pmatch (pfromData (phead # outputs)) $ \(PTxOut{ptxOut'value = valD}) -> pfromData valD

      mustNotChangeParams =
        pmustNotChangeParameters
          # pfromData prevPartiesD
          # pfromData nextPartiesD
          # pfromData prevCperiodD
          # pfromData nextCperiodD
          # prevHeadId
          # pfromData nextHeadIdD

      mustIncreaseVersion =
        ptraceIfFalse "H21" $
          nextVersion #== prevVersion + 1

      mustIncreaseValue =
        ptraceIfFalse "H4" $
          pdata (headInValue <> depositValue) #== pdata headOutValue

      signedByParticipant = pmustBeSignedByParticipant # ctx # prevHeadId

      checkSnapshotSignature =
        pverifySnapshotSignature
          # pfromData nextPartiesD
          # pfromData nextHeadIdD
          # prevVersion
          # sn
          # pfromData nextUtxoHashD
          # depositHash
          # pemptyHash
          # pfromData sigD

      claimedDepositIsSpent =
        ptraceIfFalse "H43" $
          let inputRefs = pmap # plam (\inp -> pmatch (pfromData inp) $ \(PTxInInfo{ptxInInfo'outRef}) -> ptxInInfo'outRef) # inputs
           in plistElem # incRef # inputRefs

  pcheck $
    mustNotChangeParams
      #&& mustIncreaseVersion
      #&& mustIncreaseValue
      #&& signedByParticipant
      #&& checkSnapshotSignature
      #&& claimedDepositIsSpent

-- ============================================================================
-- 9. checkDecrement
-- ============================================================================

pcheckDecrement :: Term s (PScriptContext :--> POpenDatum :--> PDecrementRedeemer :--> PUnit)
pcheckDecrement = phoistAcyclic $ plam $ \ctx openBefore redeemer -> P.do
  PScriptContext{pscriptContext'txInfo} <- pmatch ctx
  PTxInfo
    { ptxInfo'outputs
    } <-
    pmatch pscriptContext'txInfo
  POpenDatum
    { popenParties = prevPartiesD
    , popenContestationPeriod = prevCperiodD
    , popenHeadId = prevHeadIdD
    , popenVersion = prevVersionD
    } <-
    pmatch openBefore
  PDecrementRedeemer
    { pdecrementSig = sigD
    , pdecrementSnapshotNumber = snD
    , pdecrementNumberOfDecommitOutputs = numDecommitD
    } <-
    pmatch redeemer
  nextOpenDatum <- plet $ pdecodeHeadOutputOpenDatum # ctx
  POpenDatum
    { popenUtxoHash = nextUtxoHashD
    , popenParties = nextPartiesD
    , popenContestationPeriod = nextCperiodD
    , popenHeadId = nextHeadIdD
    , popenVersion = nextVersionD
    } <-
    pmatch nextOpenDatum

  let outputs = pfromData ptxInfo'outputs
      prevHeadId = pfromData prevHeadIdD
      prevVersion = pfromData prevVersionD
      nextVersion = pfromData nextVersionD
      numDecommit = pfromData numDecommitD
      sn = pfromData snD

      headOutValue = pmatch (pfromData (phead # outputs)) $ \(PTxOut{ptxOut'value = valD}) -> pfromData valD

      headInValue =
        pmatch (pfindOwnInput # ctx) $ \case
          PNothing -> mempty
          PJust ownInp -> pmatch ownInp $ \(PTxInInfo{ptxInInfo'resolved}) ->
            pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'value = valD}) -> pfromData valD

      decommitOutputs = plistTake # numDecommit # (ptail # outputs)
      decommitUtxoHash = phashTxOuts # decommitOutputs
      decommitValue = pfoldMapValues # decommitOutputs

      mustNotChangeParams =
        pmustNotChangeParameters
          # pfromData prevPartiesD
          # pfromData nextPartiesD
          # pfromData prevCperiodD
          # pfromData nextCperiodD
          # prevHeadId
          # pfromData nextHeadIdD

      mustIncreaseVersion =
        ptraceIfFalse "H21" $
          nextVersion #== prevVersion + 1

      checkSnapshotSignature =
        pverifySnapshotSignature
          # pfromData nextPartiesD
          # pfromData nextHeadIdD
          # prevVersion
          # sn
          # pfromData nextUtxoHashD
          # pemptyHash
          # decommitUtxoHash
          # pfromData sigD

      mustDecreaseValue =
        ptraceIfFalse "H4" $
          pdata headInValue #== pdata (headOutValue <> decommitValue)

      signedByParticipant = pmustBeSignedByParticipant # ctx # prevHeadId

  pcheck $
    mustNotChangeParams
      #&& mustIncreaseVersion
      #&& checkSnapshotSignature
      #&& mustDecreaseValue
      #&& signedByParticipant

-- ============================================================================
-- 10. checkClose
-- ============================================================================

pcheckClose :: Term s (PScriptContext :--> POpenDatum :--> PCloseRedeemer :--> PUnit)
pcheckClose = phoistAcyclic $ plam $ \ctx openBefore closeRed -> P.do
  PScriptContext{pscriptContext'txInfo} <- pmatch ctx
  PTxInfo
    { ptxInfo'inputs
    , ptxInfo'validRange
    , ptxInfo'signatories
    } <-
    pmatch pscriptContext'txInfo
  POpenDatum
    { popenParties = prevPartiesD
    , popenUtxoHash = initialUtxoHashD
    , popenContestationPeriod = prevCperiodD
    , popenHeadId = prevHeadIdD
    , popenVersion = prevVersionD
    } <-
    pmatch openBefore
  closedOut <- plet $ pdecodeHeadOutputClosedDatum # ctx
  PClosedDatum
    { pclosedSnapshotNumber = nextSnD
    , pclosedUtxoHash = nextUtxoHashD
    , pclosedAlphaUTxOHash = nextAlphaD
    , pclosedOmegaUTxOHash = nextOmegaD
    , pclosedParties = nextPartiesD
    , pclosedContestationDeadline = nextDeadlineD
    , pclosedContestationPeriod = nextCperiodD
    , pclosedHeadId = nextHeadIdD
    , pclosedContesters = nextContestersD
    , pclosedVersion = nextVersionD
    } <-
    pmatch closedOut
  let prevVersion = pfromData prevVersionD
      prevCperiod = pfromData prevCperiodD
      prevHeadId = pfromData prevHeadIdD
      prevParties = pfromData prevPartiesD
      nextVersion = pfromData nextVersionD

      tMax = pmatch ptxInfo'validRange $ \(PInterval _ ub) ->
        pmatch ub $ \(PUpperBound ext _) ->
          pmatch ext $ \case
            PFinite t -> pfromData (punsafeCoerce @(PAsData PInteger) t)
            _ -> ptraceInfoError "H24"

      tMin = pmatch ptxInfo'validRange $ \(PInterval lb _) ->
        pmatch lb $ \(PLowerBound ext _) ->
          pmatch ext $ \case
            PFinite t -> pfromData (punsafeCoerce @(PAsData PInteger) t)
            _ -> ptraceInfoError "H25"

      hasBoundedValidity = ptraceIfFalse "H22" (tMax - tMin #<= prevCperiod)
      noMintBurn = pmustNotMintOrBurn # pscriptContext'txInfo
      checkDeadline =
        ptraceIfFalse "H23" $
          pfromData nextDeadlineD #== tMax + prevCperiod
      signedByParticipant = pmustBeSignedByParticipant # ctx # prevHeadId
      mustNotChangeVersion = ptraceIfFalse "H13" (nextVersion #== prevVersion)
      mustInitContesters = ptraceIfFalse "H26" (pnull # pfromData nextContestersD)
      mustNotChangeParams =
        ptraceIfFalse "H2" $
          nextPartiesD
            #== prevPartiesD
            #&& nextCperiodD
            #== prevCperiodD
            #&& nextHeadIdD
            #== prevHeadIdD

      mustBeValidSnapshot = pmatch closeRed $ \case
        PCloseInitial ->
          ptraceIfFalse "H28" $
            prevVersion
              #== 0
              #&& pfromData nextSnD
              #== 0
              #&& nextUtxoHashD
              #== initialUtxoHashD
        PCloseAny{pcloseAnySig = sigD} ->
          ptraceIfFalse "H46" $
            pfromData nextSnD
              #> 0
              #&& nextAlphaD
              #== pdata pemptyHash
              #&& nextOmegaD
              #== pdata pemptyHash
              #&& pverifySnapshotSignature
              # prevParties
              # prevHeadId
              # prevVersion
              # pfromData nextSnD
              # pfromData nextUtxoHashD
              # pemptyHash
              # pemptyHash
              # pfromData sigD
        PCloseUnusedDec{pcloseUnusedDecSig = sigD} ->
          ptraceIfFalse "H50" $
            nextAlphaD
              #== pdata pemptyHash
              #&& pnot
              # (nextOmegaD #== pdata pemptyHash)
              #&& pverifySnapshotSignature
              # prevParties
              # prevHeadId
              # prevVersion
              # pfromData nextSnD
              # pfromData nextUtxoHashD
              # pemptyHash
              # pfromData nextOmegaD
              # pfromData sigD
        PCloseUsedDec{pcloseUsedDecSig = sigD, pcloseUsedDecAlreadyDecommitted = adD} ->
          ptraceIfFalse "H51" $
            nextAlphaD
              #== pdata pemptyHash
              #&& nextOmegaD
              #== pdata pemptyHash
              #&& pverifySnapshotSignature
              # prevParties
              # prevHeadId
              # (prevVersion - 1)
              # pfromData nextSnD
              # pfromData nextUtxoHashD
              # pemptyHash
              # pfromData adD
              # pfromData sigD
        PCloseUnusedInc{pcloseUnusedIncSig = sigD, pcloseUnusedIncAlreadyCommitted = acD} ->
          ptraceIfFalse "H52" $
            nextAlphaD
              #== pdata pemptyHash
              #&& nextOmegaD
              #== pdata pemptyHash
              #&& pverifySnapshotSignature
              # prevParties
              # prevHeadId
              # prevVersion
              # pfromData nextSnD
              # pfromData nextUtxoHashD
              # pfromData acD
              # pemptyHash
              # pfromData sigD
        PCloseUsedInc{pcloseUsedIncSig = sigD, pcloseUsedIncAlreadyCommitted = acD} ->
          ptraceIfFalse "H53" $
            nextAlphaD
              #== acD
              #&& nextOmegaD
              #== pdata pemptyHash
              #&& pverifySnapshotSignature
              # prevParties
              # prevHeadId
              # (prevVersion - 1)
              # pfromData nextSnD
              # pfromData nextUtxoHashD
              # pfromData acD
              # pemptyHash
              # pfromData sigD
  let preserveHeadValue = pmustPreserveHeadValue # ctx
  pcheck $
    noMintBurn
      #&& hasBoundedValidity
      #&& checkDeadline
      #&& signedByParticipant
      #&& mustNotChangeVersion
      #&& mustBeValidSnapshot
      #&& mustInitContesters
      #&& preserveHeadValue
      #&& mustNotChangeParams

-- ============================================================================
-- 11. checkContest
-- ============================================================================

pcheckContest :: Term s (PScriptContext :--> PClosedDatum :--> PContestRedeemer :--> PUnit)
pcheckContest = phoistAcyclic $ plam $ \ctx closedDatum contestRed -> P.do
  PScriptContext{pscriptContext'txInfo} <- pmatch ctx
  PTxInfo
    { ptxInfo'validRange
    , ptxInfo'signatories
    } <-
    pmatch pscriptContext'txInfo
  PClosedDatum
    { pclosedHeadId = headIdD
    , pclosedParties = partiesD
    , pclosedContestationPeriod = contestationPeriodD
    , pclosedVersion = versionD
    , pclosedSnapshotNumber = snapshotNumD
    , pclosedContesters = contestersD
    , pclosedContestationDeadline = contestationDeadlineD
    } <-
    pmatch closedDatum
  closedOut <- plet $ pdecodeHeadOutputClosedDatum # ctx
  PClosedDatum
    { pclosedSnapshotNumber = nextSnD
    , pclosedUtxoHash = nextUtxoHashD
    , pclosedAlphaUTxOHash = nextAlphaD
    , pclosedOmegaUTxOHash = nextOmegaD
    , pclosedParties = nextPartiesD
    , pclosedContestationPeriod = nextCperiodD
    , pclosedHeadId = nextHeadIdD
    , pclosedContesters = nextContestersD
    , pclosedContestationDeadline = nextDeadlineD
    , pclosedVersion = nextVersionD
    } <-
    pmatch closedOut

  let headId = pfromData headIdD
      parties = pfromData partiesD
      version = pfromData versionD
      contestationDeadline = pfromData contestationDeadlineD
      contestationPeriod = pfromData contestationPeriodD
      snapshotNum = pfromData snapshotNumD
      contesters = pfromData contestersD
      nextVersion = pfromData nextVersionD

      noMintBurn = pmustNotMintOrBurn # pscriptContext'txInfo

      mustNotChangeVersion =
        ptraceIfFalse "H13" $
          nextVersion #== version

      mustBeNewer =
        ptraceIfFalse "H29" $
          pfromData nextSnD #> snapshotNum

      mustBeValidSnapshot = pmatch contestRed $ \case
        PContestCurrent{pcontestCurrentSig = sigD} ->
          ptraceIfFalse "H37" $
            nextAlphaD
              #== pdata pemptyHash
              #&& nextOmegaD
              #== pdata pemptyHash
              #&& pverifySnapshotSignature
              # parties
              # headId
              # version
              # pfromData nextSnD
              # pfromData nextUtxoHashD
              # pemptyHash
              # pemptyHash
              # pfromData sigD
        PContestUsedDec{pcontestUsedDecSig = sigD, pcontestUsedDecAlreadyDecommitted = adD} ->
          ptraceIfFalse "H38" $
            nextAlphaD
              #== pdata pemptyHash
              #&& nextOmegaD
              #== pdata pemptyHash
              #&& pverifySnapshotSignature
              # parties
              # headId
              # (version - 1)
              # pfromData nextSnD
              # pfromData nextUtxoHashD
              # pemptyHash
              # pfromData adD
              # pfromData sigD
        PContestUnusedDec{pcontestUnusedDecSig = sigD} ->
          ptraceIfFalse "H47" $
            nextAlphaD
              #== pdata pemptyHash
              #&& pverifySnapshotSignature
              # parties
              # headId
              # version
              # pfromData nextSnD
              # pfromData nextUtxoHashD
              # pemptyHash
              # pfromData nextOmegaD
              # pfromData sigD
        PContestUnusedInc{pcontestUnusedIncSig = sigD, pcontestUnusedIncAlreadyCommitted = acD} ->
          ptraceIfFalse "H48" $
            nextOmegaD
              #== pdata pemptyHash
              #&& pverifySnapshotSignature
              # parties
              # headId
              # (version - 1)
              # pfromData nextSnD
              # pfromData nextUtxoHashD
              # pfromData acD
              # pemptyHash
              # pfromData sigD
        PContestUsedInc{pcontestUsedIncSig = sigD} ->
          ptraceIfFalse "H49" $
            nextOmegaD
              #== pdata pemptyHash
              #&& pverifySnapshotSignature
              # parties
              # headId
              # version
              # pfromData nextSnD
              # pfromData nextUtxoHashD
              # pfromData nextAlphaD
              # pemptyHash
              # pfromData sigD

      signedByParticipant = pmustBeSignedByParticipant # ctx # headId

      -- contester = single signer (kept as PAsData PPubKeyHash to match list type)
      contester =
        pelimList
          ( \signer rest ->
              pelimList
                (\_ _ -> ptraceInfoError "H35")
                signer
                rest
          )
          (ptraceInfoError "H35")
          (pfromData ptxInfo'signatories)

      checkSignedParticipantContestOnlyOnce =
        let goNotElem = pfix #$ plam $ \self x xs ->
              pelimList
                (\h t -> pif (h #== x) (pconstant False) (self # x # t))
                (pconstant True)
                xs
         in ptraceIfFalse "H36" $
              goNotElem # contester # contesters

      mustBeWithinContestationPeriod =
        pmatch ptxInfo'validRange $ \(PInterval _ ub) ->
          pmatch ub $ \(PUpperBound ext _) ->
            pmatch ext $ \case
              PFinite t ->
                ptraceIfFalse "H30" $
                  pfromData (punsafeCoerce @(PAsData PInteger) t) #<= contestationDeadline
              _ -> ptraceInfoError "H31"

      mustUpdateContesters =
        ptraceIfFalse "H34" $
          pfromData nextContestersD #== (pcons # contester # contesters)

      mustPushDeadline =
        pif
          (plistLength # pfromData nextContestersD #== plistLength # parties)
          ( ptraceIfFalse "H32" $
              pfromData nextDeadlineD #== contestationDeadline
          )
          ( ptraceIfFalse "H33" $
              pfromData nextDeadlineD #== contestationDeadline + contestationPeriod
          )

      mustNotChangeParams =
        pmustNotChangeParameters
          # pfromData partiesD
          # pfromData nextPartiesD
          # contestationPeriod
          # pfromData nextCperiodD
          # headId
          # pfromData nextHeadIdD

      preserveHeadValue = pmustPreserveHeadValue # ctx

  pcheck $
    noMintBurn
      #&& mustNotChangeVersion
      #&& mustBeNewer
      #&& mustBeValidSnapshot
      #&& signedByParticipant
      #&& checkSignedParticipantContestOnlyOnce
      #&& mustBeWithinContestationPeriod
      #&& mustUpdateContesters
      #&& mustPushDeadline
      #&& mustNotChangeParams
      #&& preserveHeadValue

-- ============================================================================
-- 12. headIsFinalizedWith (Fanout)
-- ============================================================================

pheadIsFinalizedWith :: Term s (PScriptContext :--> PClosedDatum :--> PInteger :--> PInteger :--> PInteger :--> PUnit)
pheadIsFinalizedWith = phoistAcyclic $ plam $ \ctx closedDatum numFanout numCommit numDecommit -> P.do
  PScriptContext{pscriptContext'txInfo} <- pmatch ctx
  PTxInfo{ptxInfo'mint, ptxInfo'outputs, ptxInfo'validRange} <- pmatch pscriptContext'txInfo
  PClosedDatum
    { pclosedHeadId = headIdD
    , pclosedParties = partiesD
    , pclosedUtxoHash = utxoHashD
    , pclosedAlphaUTxOHash = alphaHashD
    , pclosedOmegaUTxOHash = omegaHashD
    , pclosedContestationDeadline = deadlineD
    } <-
    pmatch closedDatum

  let mintVal = punsafeCoerce @(PValue 'Sorted 'NoGuarantees) (pfromData ptxInfo'mint)
      outputs = pfromData ptxInfo'outputs
      headId = pfromData headIdD
      parties = pfromData partiesD
      utxoHash = pfromData utxoHashD
      alphaHash = pfromData alphaHashD
      omegaHash = pfromData omegaHashD
      deadline = pfromData deadlineD

      burnOk = pmustBurnAllHeadTokens # mintVal # headId # parties

      fanoutOutputs = plistTake # numFanout # outputs
      fannedOutHash = phashTxOuts # fanoutOutputs
      hasSameUtxoHash = ptraceIfFalse "H39" (fannedOutHash #== utxoHash)

      commitOutputs = plistTake # numCommit # (plistDrop # numFanout # outputs)
      commitHash = phashTxOuts # commitOutputs
      hasSameCommitHash = ptraceIfFalse "H54" (pdata alphaHash #== pdata commitHash)

      decommitOutputs = plistTake # numDecommit # (plistDrop # numFanout # outputs)
      decommitHash = phashTxOuts # decommitOutputs
      hasSameDecommitHash = ptraceIfFalse "H40" (pdata omegaHash #== pdata decommitHash)

      afterDeadline =
        pmatch ptxInfo'validRange $ \(PInterval lb _) ->
          pmatch lb $ \(PLowerBound ext _) ->
            pmatch ext $ \case
              PFinite t -> ptraceIfFalse "H41" (pfromData (punsafeCoerce @(PAsData PInteger) t) #> deadline)
              _ -> ptraceInfoError "H42"

  pcheck $
    burnOk
      #&& hasSameUtxoHash
      #&& hasSameCommitHash
      #&& hasSameDecommitHash
      #&& afterDeadline

-- ============================================================================
-- 13. Main validator
-- ============================================================================

mkHeadValidator :: Term s (PScriptContext :--> PUnit)
mkHeadValidator = plam $ \ctx ->
  pmatch ctx $ \(PScriptContext{pscriptContext'redeemer, pscriptContext'scriptInfo}) ->
    pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum ->
        pmatch mDatum $ \case
          PDJust datumRaw ->
            let state = pfromData $ punsafeCoerce @(PAsData PState) (pto (pfromData datumRaw))
                input = pfromData $ punsafeCoerce @(PAsData PInput) (pto pscriptContext'redeemer)
             in pmatch state $ \case
                  POpen openAsData ->
                    let openDatum = pfromData openAsData
                     in pmatch input $ \case
                          PIncrement{pinput'increment = incD} -> pcheckIncrement # ctx # openDatum # pfromData incD
                          PDecrement{pinput'decrement = decD} -> pcheckDecrement # ctx # openDatum # pfromData decD
                          PClose{pinput'close = closeD} -> pcheckClose # ctx # openDatum # pfromData closeD
                          _ -> ptraceInfoError "H1"
                  PClosed closedAsData ->
                    let closedDatum = pfromData closedAsData
                     in pmatch input $ \case
                          PContest{pinput'contest = contestD} -> pcheckContest # ctx # closedDatum # pfromData contestD
                          PFanout{pfanoutNumberOfFanoutOutputs, pfanoutNumberOfCommitOutputs, pfanoutNumberOfDecommitOutputs} ->
                            pheadIsFinalizedWith # ctx # closedDatum # pfromData pfanoutNumberOfFanoutOutputs # pfromData pfanoutNumberOfCommitOutputs # pfromData pfanoutNumberOfDecommitOutputs
                          _ -> ptraceInfoError "H1"
                  PFinal -> ptraceInfoError "H1"
          PDNothing -> perror
      _ -> perror
