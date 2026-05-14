{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Crowdfund.Contracts.Crowdfund (
  mkCrowdfundValidator,
) where

import Plutarch.LedgerApi.AssocMap (plookup)
import Plutarch.LedgerApi.V3
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Crowdfund.Types.CrowdfundState (PCrowdfundDatum (..), PCrowdfundRedeemer (..))

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

pcheck :: Term s PBool -> Term s PUnit
pcheck b = pif b (pconstant ()) perror

plistLength :: (PIsListLike l a) => Term s (l a :--> PInteger)
plistLength = phoistAcyclic $ pfix #$ plam $ \self xs ->
  pelimList (\_ rest -> 1 + self # rest) 0 xs

-- ============================================================================
-- 2. Utility functions
-- ============================================================================

pfindOwnInput :: Term s (PScriptContext :--> PTxInInfo)
pfindOwnInput = phoistAcyclic $ plam $ \ctx ->
  pmatch ctx $ \(PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo}) ->
    pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript ref _ ->
        let txInputs = pfromData $ pmatch pscriptContext'txInfo $ \txI -> ptxInfo'inputs txI
            go = pfix #$ plam $ \self inputs ->
              pelimList
                ( \inp rest ->
                    pmatch (pfromData inp) $ \(PTxInInfo{ptxInInfo'outRef}) ->
                      pif (ptxInInfo'outRef #== ref) (pfromData inp) (self # rest)
                )
                perror
                inputs
         in go # txInputs
      _ -> perror

pgetInputsByAddress
  :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PAddress :--> PBuiltinList (PAsData PTxInInfo))
pgetInputsByAddress = phoistAcyclic $ pfix #$ plam $ \self inputs addr ->
  pelimList
    ( \inp rest ->
        pmatch (pfromData inp) $ \(PTxInInfo{ptxInInfo'resolved}) ->
          pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'address}) ->
            pif
              (ptxOut'address #== addr)
              (pcons # inp # (self # rest # addr))
              (self # rest # addr)
    )
    pnil
    inputs

pgetOutputsByAddress
  :: Term s (PBuiltinList (PAsData PTxOut) :--> PAddress :--> PBuiltinList (PAsData PTxOut))
pgetOutputsByAddress = phoistAcyclic $ pfix #$ plam $ \self outputs addr ->
  pelimList
    ( \out rest ->
        pmatch (pfromData out) $ \(PTxOut{ptxOut'address}) ->
          pif
            (ptxOut'address #== addr)
            (pcons # out # (self # rest # addr))
            (self # rest # addr)
    )
    pnil
    outputs

pgetLovelace :: Term s (PValue 'Sorted 'Positive :--> PInteger)
pgetLovelace = phoistAcyclic $ plam $ \v ->
  let entries = pto (pto v)
   in pelimList
        ( \first _ ->
            let tokens = pto (pfromData (psndBuiltin # first))
             in pelimList
                  (\tk _ -> pfromData (psndBuiltin # tk))
                  0
                  tokens
        )
        0
        entries

pgetAdaFromInputs :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PInteger)
pgetAdaFromInputs = phoistAcyclic $ pfix #$ plam $ \self inputs ->
  pelimList
    ( \inp rest ->
        pmatch (pfromData inp) $ \(PTxInInfo{ptxInInfo'resolved}) ->
          pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'value}) ->
            pgetLovelace # pfromData ptxOut'value + self # rest
    )
    0
    inputs

pgetAdaFromOutputs :: Term s (PBuiltinList (PAsData PTxOut) :--> PInteger)
pgetAdaFromOutputs = phoistAcyclic $ pfix #$ plam $ \self outputs ->
  pelimList
    ( \out rest ->
        pmatch (pfromData out) $ \(PTxOut{ptxOut'value}) ->
          pgetLovelace # pfromData ptxOut'value + self # rest
    )
    0
    outputs

pmustStartBeforeTimeout :: Term s (PInterval PPosixTime :--> PInteger :--> PBool)
pmustStartBeforeTimeout = phoistAcyclic $ plam $ \range deadline ->
  pmatch range $ \(PInterval lb _) ->
    pmatch lb $ \(PLowerBound ext _) ->
      pmatch ext $ \case
        PFinite t -> pfromData (punsafeCoerce @(PAsData PInteger) t) #< deadline
        _ -> pconstant False

pmustBeSignedBy :: Term s (PTxInfo :--> PPubKeyHash :--> PBool)
pmustBeSignedBy = phoistAcyclic $ plam $ \txInfo pkh ->
  pmatch txInfo $ \txI ->
    let signatories = pfromData $ ptxInfo'signatories txI
        go = pfix #$ plam $ \self sigs ->
          pelimList
            (\s rest -> pif (pfromData s #== pkh) (pconstant True) (self # rest))
            (pconstant False)
            sigs
     in go # signatories

psumWallets :: Term s (PMap any PPubKeyHash PInteger :--> PInteger)
psumWallets = phoistAcyclic $ plam $ \m ->
  let go = pfix #$ plam $ \self pairs ->
        pelimList
          (\pair rest -> pfromData (psndBuiltin # pair) + self # rest)
          0
          pairs
   in go # pto m

pmapSize :: Term s (PMap any PPubKeyHash PInteger :--> PInteger)
pmapSize = phoistAcyclic $ plam $ \m -> plistLength # pto m

pfilterOutKey
  :: forall (any :: KeyGuarantees) s
   . Term
       s
       ( PPubKeyHash
           :--> PMap any PPubKeyHash PInteger
           :--> PMap any PPubKeyHash PInteger
       )
pfilterOutKey = phoistAcyclic $ plam $ \key m ->
  let goPairs = pfix #$ plam $ \self pairs ->
        pelimList
          ( \pair rest ->
              pif
                (pfromData (pfstBuiltin # pair) #== key)
                (self # rest)
                (pcons # pair # (self # rest))
          )
          pnil
          pairs
   in pcon $ PMap (goPairs # pto m)

pgetOutputDatum :: Term s (PTxOut :--> PCrowdfundDatum)
pgetOutputDatum = phoistAcyclic $ plam $ \txOut ->
  pmatch txOut $ \(PTxOut{ptxOut'datum}) ->
    pmatch ptxOut'datum $ \case
      POutputDatum d ->
        pfromData $ punsafeCoerce @(PAsData PCrowdfundDatum) (pto d)
      _ -> perror

-- ============================================================================
-- 3. Donate
-- ============================================================================

pcheckDonate
  :: Term
       s
       ( PCrowdfundDatum
           :--> PTxInfo
           :--> PBuiltinList (PAsData PTxOut)
           :--> PInteger
           :--> PInteger
           :--> PPubKeyHash
           :--> PBool
       )
pcheckDonate = phoistAcyclic $ plam $ \datum txInfo contractOutputs contractAmount amount donor ->
  plistLength
    # contractOutputs
    #== 1
    #&& plet
      (pfromData (phead # contractOutputs))
      ( \contractOutput ->
          plet (pgetOutputDatum # contractOutput) $ \outputDatum ->
            pmatch datum $ \(PCrowdfundDatum{pcfRecipient, pcfGoal, pcfDeadline, pcfWallets}) ->
              pmatch outputDatum $
                \( PCrowdfundDatum
                     { pcfRecipient = outRecipient
                     , pcfGoal = outGoal
                     , pcfDeadline = outDeadline
                     , pcfWallets = outWallets
                     }
                   ) ->
                    let inputWallets = pfromData pcfWallets
                        outputWallets = pfromData outWallets
                        validRange = pmatch txInfo $ \txI -> ptxInfo'validRange txI
                        deadline = pfromData pcfDeadline
                        walletsUnchanged =
                          pdata (pfilterOutKey # donor # inputWallets)
                            #== pdata (pfilterOutKey # donor # outputWallets)
                        donorAmountCorrect =
                          pmatch (plookup # donor # outputWallets) $ \case
                            PJust outputWalletAmount ->
                              pmatch (plookup # donor # inputWallets) $ \case
                                PNothing -> amount #== outputWalletAmount
                                PJust previousAmount -> outputWalletAmount #== previousAmount + amount
                            PNothing -> pconstant False
                     in pmustStartBeforeTimeout
                          # validRange
                          # deadline
                          #&& pmustBeSignedBy
                          # txInfo
                          # donor
                          #&& pgetAdaFromOutputs
                          # contractOutputs
                          #== contractAmount
                          + amount
                            #&& outRecipient
                            #== pcfRecipient
                            #&& outGoal
                            #== pcfGoal
                            #&& outDeadline
                            #== pcfDeadline
                            #&& walletsUnchanged
                            #&& donorAmountCorrect
      )

-- ============================================================================
-- 4. Withdraw
-- ============================================================================

pcheckWithdraw :: Term s (PCrowdfundDatum :--> PTxInfo :--> PInteger :--> PBool)
pcheckWithdraw = phoistAcyclic $ plam $ \datum txInfo contractAmount ->
  pmatch datum $ \(PCrowdfundDatum{pcfRecipient, pcfGoal, pcfDeadline}) ->
    let validRange = pmatch txInfo $ \txI -> ptxInfo'validRange txI
        deadline = pfromData pcfDeadline
        goal = pfromData pcfGoal
     in pmustBeSignedBy
          # txInfo
          # pfromData pcfRecipient
          #&& pnot
          # (pmustStartBeforeTimeout # validRange # deadline)
          #&& contractAmount
          #>= goal

-- ============================================================================
-- 5. Reclaim
-- ============================================================================

pcheckReclaim
  :: Term
       s
       ( PCrowdfundDatum
           :--> PTxInfo
           :--> PBuiltinList (PAsData PTxInInfo)
           :--> PBuiltinList (PAsData PTxOut)
           :--> PBool
       )
pcheckReclaim = phoistAcyclic $ plam $ \datum txInfo contractInputs contractOutputs ->
  pmatch datum $ \(PCrowdfundDatum{pcfRecipient, pcfGoal, pcfDeadline, pcfWallets}) ->
    let validRange = pmatch txInfo $ \txI -> ptxInfo'validRange txI
        deadline = pfromData pcfDeadline
        wallets = pfromData pcfWallets
        signatories = pmatch txInfo $ \txI -> pfromData $ ptxInfo'signatories txI
        currentSigner = pfromData (phead # signatories)
     in pmatch (plookup # currentSigner # wallets) $ \case
          PNothing -> pconstant False
          PJust withdrawAmount ->
            pnot
              # (pmustStartBeforeTimeout # validRange # deadline)
              #&& pif
                (pmapSize # wallets #> 1)
                ( plistLength
                    # contractOutputs
                    #== 1
                    #&& plet
                      (pfromData (phead # contractOutputs))
                      ( \contractOutput ->
                          plet (pgetOutputDatum # contractOutput) $ \outputDatum ->
                            pmatch outputDatum $
                              \( PCrowdfundDatum
                                   { pcfRecipient = outRecipient
                                   , pcfGoal = outGoal
                                   , pcfDeadline = outDeadline
                                   , pcfWallets = outWallets
                                   }
                                 ) ->
                                  pgetAdaFromOutputs # contractOutputs #== pgetAdaFromInputs # contractInputs
                                    - withdrawAmount
                                      #&& outRecipient
                                      #== pcfRecipient
                                      #&& outGoal
                                      #== pcfGoal
                                      #&& outDeadline
                                      #== pcfDeadline
                                      #&& outWallets
                                      #== pdata (pfilterOutKey # currentSigner # wallets)
                      )
                )
                (plistLength # contractOutputs #== 0)

-- ============================================================================
-- 6. Main validator
-- ============================================================================

pcrowdfundValidator
  :: Term s (PScriptContext :--> PCrowdfundDatum :--> PCrowdfundRedeemer :--> PUnit)
pcrowdfundValidator = phoistAcyclic $ plam $ \ctx datum redeemer ->
  pmatch ctx $ \(PScriptContext{pscriptContext'txInfo}) ->
    let txInfo = pscriptContext'txInfo
        ownInput = pfindOwnInput # ctx
        contractAddress = pmatch ownInput $ \(PTxInInfo{ptxInInfo'resolved}) ->
          pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'address}) -> ptxOut'address
        inputs = pfromData $ pmatch txInfo $ \txI -> ptxInfo'inputs txI
        outputs = pfromData $ pmatch txInfo $ \txI -> ptxInfo'outputs txI
        contractInputs = pgetInputsByAddress # inputs # contractAddress
        contractOutputs = pgetOutputsByAddress # outputs # contractAddress
     in pmatch datum $ \(PCrowdfundDatum{pcfWallets}) ->
          plet (psumWallets # pfromData pcfWallets) $ \contractAmount ->
            pcheck $
              contractAmount
                #== pgetAdaFromInputs
                # contractInputs
                #&& pmatch
                  redeemer
                  ( \case
                      PDonate{pdonateAmount, pdonateDonor} ->
                        pcheckDonate
                          # datum
                          # txInfo
                          # contractOutputs
                          # contractAmount
                          # pfromData pdonateAmount
                          # pfromData pdonateDonor
                      PWithdraw ->
                        pcheckWithdraw # datum # txInfo # contractAmount
                      PReclaim ->
                        pcheckReclaim # datum # txInfo # contractInputs # contractOutputs
                  )

-- ============================================================================
-- 7. Entry point
-- ============================================================================

mkCrowdfundValidator :: Term s (PScriptContext :--> PUnit)
mkCrowdfundValidator = plam $ \ctx ->
  pmatch ctx $ \(PScriptContext{pscriptContext'redeemer, pscriptContext'scriptInfo}) ->
    pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum ->
        pmatch mDatum $ \case
          PDJust datumRaw ->
            let datum = pfromData $ punsafeCoerce @(PAsData PCrowdfundDatum) (pto (pfromData datumRaw))
                redeemer = pfromData $ punsafeCoerce @(PAsData PCrowdfundRedeemer) (pto pscriptContext'redeemer)
             in pcrowdfundValidator # ctx # datum # redeemer
          PDNothing -> perror
      _ -> perror
