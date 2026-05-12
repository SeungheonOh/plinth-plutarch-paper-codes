{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Vesting.Contracts.Vesting (
  mkVestingValidator,
) where

import Plutarch.LedgerApi.V3
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Vesting.Types.VestingState (PVestingDatum (..), PVestingRedeemer (..))

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

pgetOutputsByAddress :: Term s (PBuiltinList (PAsData PTxOut) :--> PAddress :--> PBuiltinList (PAsData PTxOut))
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

pcredentialMatchesVkh :: Term s (PData :--> PPubKeyHash :--> PBool)
pcredentialMatchesVkh = phoistAcyclic $ plam $ \credData vkh ->
  plet (pasConstr # credData) $ \pair ->
    pif
      (pfstBuiltin # pair #== 0)
      ( pelimList
          (\pkhData _ -> pfromData (punsafeCoerce @(PAsData PPubKeyHash) pkhData) #== vkh)
          (pconstant False)
          (psndBuiltin # pair)
      )
      (pconstant False)

pgetOutputsByVkh :: Term s (PBuiltinList (PAsData PTxOut) :--> PPubKeyHash :--> PBuiltinList (PAsData PTxOut))
pgetOutputsByVkh = phoistAcyclic $ pfix #$ plam $ \self outputs vkh ->
  pelimList
    ( \out rest ->
        let outFields = psndBuiltin # (pasConstr # pforgetData out)
            addrFields = psndBuiltin # (pasConstr # (phead # outFields))
            credData = phead # addrFields
         in pif
              (pcredentialMatchesVkh # credData # vkh)
              (pcons # out # (self # rest # vkh))
              (self # rest # vkh)
    )
    pnil
    outputs

pgetInputsByVkh :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PPubKeyHash :--> PBuiltinList (PAsData PTxInInfo))
pgetInputsByVkh = phoistAcyclic $ pfix #$ plam $ \self inputs vkh ->
  pelimList
    ( \inp rest ->
        let inpFields = psndBuiltin # (pasConstr # pforgetData inp)
            resolvedData = phead # (ptail # inpFields)
            outFields = psndBuiltin # (pasConstr # resolvedData)
            addrFields = psndBuiltin # (pasConstr # (phead # outFields))
            credData = phead # addrFields
         in pif
              (pcredentialMatchesVkh # credData # vkh)
              (pcons # inp # (self # rest # vkh))
              (self # rest # vkh)
    )
    pnil
    inputs

pgetLovelaceAmount :: Term s (PValue 'Sorted 'Positive :--> PInteger)
pgetLovelaceAmount = phoistAcyclic $ plam $ \v ->
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
            pgetLovelaceAmount # pfromData ptxOut'value + self # rest
    )
    0
    inputs

pgetAdaFromOutputs :: Term s (PBuiltinList (PAsData PTxOut) :--> PInteger)
pgetAdaFromOutputs = phoistAcyclic $ pfix #$ plam $ \self outputs ->
  pelimList
    ( \out rest ->
        pmatch (pfromData out) $ \(PTxOut{ptxOut'value}) ->
          pgetLovelaceAmount # pfromData ptxOut'value + self # rest
    )
    0
    outputs

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

pgetEarliestTime :: Term s (PTxInfo :--> PInteger)
pgetEarliestTime = phoistAcyclic $ plam $ \txInfo ->
  pmatch txInfo $ \txI ->
    pmatch (ptxInfo'validRange txI) $ \(PInterval lb _) ->
      pmatch lb $ \(PLowerBound ext _) ->
        pmatch ext $ \case
          PFinite t -> pfromData (punsafeCoerce @(PAsData PInteger) t)
          _ -> 0

pgetFee :: Term s (PTxInfo :--> PInteger)
pgetFee = phoistAcyclic $ plam $ \txInfo ->
  pmatch txInfo $ \txI ->
    pasInt # pforgetData (ptxInfo'fee txI)

pgetOutputDatum :: Term s (PTxOut :--> PVestingDatum)
pgetOutputDatum = phoistAcyclic $ plam $ \txOut ->
  pmatch txOut $ \(PTxOut{ptxOut'datum}) ->
    pmatch ptxOut'datum $ \case
      POutputDatum d ->
        pfromData $ punsafeCoerce @(PAsData PVestingDatum) (pto d)
      _ -> perror

-- ============================================================================
-- 3. Linear vesting formula
-- ============================================================================

plinearVesting :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger)
plinearVesting = phoistAcyclic $ plam $ \startTimestamp duration totalAllocation timestamp ->
  pif
    (timestamp #< startTimestamp)
    0
    ( pif
        (timestamp #> startTimestamp + duration)
        totalAllocation
        (pdiv # (totalAllocation * (timestamp - startTimestamp)) # duration)
    )

-- ============================================================================
-- 4. Main validator logic
-- ============================================================================

pvestingValidator :: Term s (PScriptContext :--> PVestingDatum :--> PVestingRedeemer :--> PUnit)
pvestingValidator = phoistAcyclic $ plam $ \ctx datum redeemer ->
  pmatch ctx $ \(PScriptContext{pscriptContext'txInfo}) ->
    let txInfo = pscriptContext'txInfo
        ownInput = pfindOwnInput # ctx
        contractAddress = pmatch ownInput $ \(PTxInInfo{ptxInInfo'resolved}) ->
          pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'address}) -> ptxOut'address
        inputs = pfromData $ pmatch txInfo $ \txI -> ptxInfo'inputs txI
        outputs = pfromData $ pmatch txInfo $ \txI -> ptxInfo'outputs txI
        contractOutputs = pgetOutputsByAddress # outputs # contractAddress
        contractAmount = pmatch ownInput $ \(PTxInInfo{ptxInInfo'resolved}) ->
          pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'value}) ->
            pgetLovelaceAmount # pfromData ptxOut'value
     in pmatch datum $ \(PVestingDatum{pvdBeneficiary, pvdStartTimestamp, pvdDuration, pvdAmount}) ->
          let beneficiary = pfromData pvdBeneficiary
              startTimestamp = pfromData pvdStartTimestamp
              duration = pfromData pvdDuration
              totalAmount = pfromData pvdAmount
              beneficiaryInputs = pgetInputsByVkh # inputs # beneficiary
              beneficiaryOutputs = pgetOutputsByVkh # outputs # beneficiary
              txEarliestTime = pgetEarliestTime # txInfo
              released = totalAmount - contractAmount
              releaseAmount =
                plinearVesting # startTimestamp # duration # totalAmount # txEarliestTime - released
              fee = pgetFee # txInfo
           in pmatch redeemer $ \(PRelease declaredAmountData) ->
                let declaredAmount = pfromData declaredAmountData
                 in pcheck $
                      pmustBeSignedBy
                        # txInfo
                        # beneficiary
                        #&& declaredAmount
                        #== releaseAmount
                        #&& pgetAdaFromOutputs
                        # beneficiaryOutputs
                        #== declaredAmount
                        + pgetAdaFromInputs # beneficiaryInputs
                        - fee
                          #&& pif
                            (declaredAmount #== contractAmount)
                            (pconstant True)
                            ( plistLength
                                # contractOutputs
                                #== 1
                                #&& plet
                                  (pfromData (phead # contractOutputs))
                                  ( \contractOutput ->
                                      plet (pgetOutputDatum # contractOutput) $ \outputDatum ->
                                        outputDatum #== datum
                                  )
                            )

-- ============================================================================
-- 5. Entry point
-- ============================================================================

mkVestingValidator :: Term s (PScriptContext :--> PUnit)
mkVestingValidator = plam $ \ctx ->
  pmatch ctx $ \(PScriptContext{pscriptContext'redeemer, pscriptContext'scriptInfo}) ->
    pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum ->
        pmatch mDatum $ \case
          PDJust datumRaw ->
            let datum = pfromData $ punsafeCoerce @(PAsData PVestingDatum) (pto (pfromData datumRaw))
                redeemer = pfromData $ punsafeCoerce @(PAsData PVestingRedeemer) (pto pscriptContext'redeemer)
             in pvestingValidator # ctx # datum # redeemer
          PDNothing -> perror
      _ -> perror
