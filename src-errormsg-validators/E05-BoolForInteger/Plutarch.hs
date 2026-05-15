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

pfindOwnInput :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PTxOutRef :--> PTxInInfo)
pfindOwnInput = phoistAcyclic $ pfix #$ plam $ \self inputs ref ->
  pelimList
    ( \inp rest ->
        pmatch (pfromData inp) $ \(PTxInInfo{ptxInInfo'outRef}) ->
          pif (ptxInInfo'outRef #== ref) (pfromData inp) (self # rest # ref)
    )
    perror
    inputs

pcredentialMatchesVkh :: Term s (PCredential :--> PPubKeyHash :--> PBool)
pcredentialMatchesVkh = phoistAcyclic $ plam $ \cred vkh ->
  pmatch cred $ \case
    PPubKeyCredential pkh -> pfromData pkh #== vkh
    _ -> pconstant False

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

-- | Single pass through outputs: sum ADA of outputs at PubKeyCredential `vkh`.
pfoldAdaByVkhOutputs
  :: Term s (PBuiltinList (PAsData PTxOut) :--> PPubKeyHash :--> PInteger)
pfoldAdaByVkhOutputs = phoistAcyclic $ pfix #$ plam $ \self outputs vkh ->
  pelimList
    ( \out rest ->
        pmatch (pfromData out) $ \(PTxOut{ptxOut'address, ptxOut'value}) ->
          pmatch ptxOut'address $ \(PAddress{paddress'credential}) ->
            pif
              (pcredentialMatchesVkh # paddress'credential # vkh)
              (pgetLovelaceAmount # pfromData ptxOut'value + (self # rest # vkh))
              (self # rest # vkh)
    )
    0
    outputs

{- | Single pass through inputs: sum ADA of inputs whose resolved output
is at PubKeyCredential `vkh`.
-}
pfoldAdaByVkhInputs
  :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PPubKeyHash :--> PInteger)
pfoldAdaByVkhInputs = phoistAcyclic $ pfix #$ plam $ \self inputs vkh ->
  pelimList
    ( \inp rest ->
        pmatch (pfromData inp) $ \(PTxInInfo{ptxInInfo'resolved}) ->
          pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'address, ptxOut'value}) ->
            pmatch ptxOut'address $ \(PAddress{paddress'credential}) ->
              pif
                (pcredentialMatchesVkh # paddress'credential # vkh)
                (pgetLovelaceAmount # pfromData ptxOut'value + (self # rest # vkh))
                (self # rest # vkh)
    )
    0
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

pmustBeSignedBy
  :: Term s (PBuiltinList (PAsData PPubKeyHash) :--> PPubKeyHash :--> PBool)
pmustBeSignedBy = phoistAcyclic $ pfix #$ plam $ \self sigs pkh ->
  pelimList
    (\s rest -> pif (pfromData s #== pkh) (pconstant True) (self # rest # pkh))
    (pconstant False)
    sigs

pgetEarliestTime :: Term s (PInterval PPosixTime :--> PInteger)
pgetEarliestTime = phoistAcyclic $ plam $ \validRange ->
  pmatch validRange $ \(PInterval lb _) ->
    pmatch lb $ \(PLowerBound ext _) ->
      pmatch ext $ \case
        PFinite t -> pfromData (punsafeCoerce @(PAsData PInteger) t)
        _ -> 0

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

plinearVesting
  :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger)
plinearVesting = phoistAcyclic $ plam $ \startTimestamp duration totalAllocation timestamp ->
  pif
    (timestamp #< startTimestamp)
    (pconstant True)
    ( pif
        (timestamp #> startTimestamp + duration)
        totalAllocation
        (pdiv # (totalAllocation * (timestamp - startTimestamp)) # duration)
    )

-- ============================================================================
-- 4. Main validator
-- ============================================================================

mkVestingValidator :: Term s (PScriptContext :--> PUnit)
mkVestingValidator = plam $ \ctx ->
  pmatch ctx $
    \( PScriptContext
         { pscriptContext'txInfo
         , pscriptContext'redeemer
         , pscriptContext'scriptInfo
         }
       ) ->
        pmatch pscriptContext'scriptInfo $ \case
          PSpendingScript ownRef mDatum ->
            pmatch mDatum $ \case
              PDJust datumRaw -> unTermCont $ do
                let datum =
                      pfromData $
                        punsafeCoerce @(PAsData PVestingDatum) (pto (pfromData datumRaw))
                    redeemer =
                      pfromData $
                        punsafeCoerce @(PAsData PVestingRedeemer) (pto pscriptContext'redeemer)
                PVestingDatum
                  { pvdBeneficiary
                  , pvdStartTimestamp
                  , pvdDuration
                  , pvdAmount
                  } <-
                  pmatchC datum
                let beneficiary = pfromData pvdBeneficiary
                    startTimestamp = pfromData pvdStartTimestamp
                    duration = pfromData pvdDuration
                    totalAmount = pfromData pvdAmount
                PTxInfo
                  { ptxInfo'inputs
                  , ptxInfo'outputs
                  , ptxInfo'fee
                  , ptxInfo'validRange
                  , ptxInfo'signatories
                  } <-
                  pmatchC pscriptContext'txInfo
                pure $
                  pif
                    (pmustBeSignedBy # pfromData ptxInfo'signatories # beneficiary)
                    ( unTermCont $ do
                        inputs <- pletC (pfromData ptxInfo'inputs)
                        outputs <- pletC (pfromData ptxInfo'outputs)
                        PTxInInfo{ptxInInfo'resolved} <-
                          pmatchC (pfindOwnInput # inputs # ownRef)
                        ownResolved <- pletC ptxInInfo'resolved
                        PTxOut{ptxOut'address, ptxOut'value} <- pmatchC ownResolved
                        contractAmount <-
                          pletC (pgetLovelaceAmount # pfromData ptxOut'value)
                        PRelease declaredAmountData <- pmatchC redeemer
                        declaredAmount <- pletC (pfromData declaredAmountData)
                        let txEarliestTime = pgetEarliestTime # ptxInfo'validRange
                            fee = pasInt # pforgetData ptxInfo'fee
                            released = totalAmount - contractAmount
                            releaseAmount =
                              plinearVesting
                                # startTimestamp
                                # duration
                                # totalAmount
                                # txEarliestTime
                                - released
                            beneAdaOut = pfoldAdaByVkhOutputs # outputs # beneficiary
                            beneAdaIn = pfoldAdaByVkhInputs # inputs # beneficiary
                            checkContractOutput =
                              pif
                                (declaredAmount #== contractAmount)
                                (pconstant True)
                                ( plet (pgetOutputsByAddress # outputs # ptxOut'address) $
                                    \contractOutputs ->
                                      plistLength
                                        # contractOutputs
                                        #== 1
                                        #&& plet
                                          (pfromData (phead # contractOutputs))
                                          ( \contractOutput ->
                                              plet (pgetOutputDatum # contractOutput) $
                                                \outputDatum -> outputDatum #== datum
                                          )
                                )
                        pure $
                          pcheck $
                            declaredAmount
                              #== releaseAmount
                              #&& beneAdaOut
                              #== declaredAmount
                              + beneAdaIn
                              - fee
                                #&& checkContractOutput
                    )
                    perror
              PDNothing -> perror
          _ -> perror
