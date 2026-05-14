{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Settings.Contracts.Settings (
  mkSettingsValidator,
) where

import Plutarch.LedgerApi.V3
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V3 (CurrencySymbol (..), TokenName (..))

import Settings.Types.SettingsState (
  PMultisigScript (..),
  PSettingsDatum (..),
  PSettingsRedeemer (..),
 )

-- ============================================================================
-- 1. Infrastructure
-- ============================================================================

pcheck :: Term s PBool -> Term s PUnit
pcheck b = pif b (pconstant ()) perror

-- ============================================================================
-- 2. Multisig
-- ============================================================================

plistElem :: Term s (PPubKeyHash :--> PBuiltinList (PAsData PPubKeyHash) :--> PBool)
plistElem = phoistAcyclic $ pfix #$ plam $ \self key xs ->
  pelimList
    (\x rest -> pif (pfromData x #== key) (pconstant True) (self # key # rest))
    (pconstant False)
    xs

plistAll
  :: Term s ((PAsData PMultisigScript :--> PBool) :--> PBuiltinList (PAsData PMultisigScript) :--> PBool)
plistAll = phoistAcyclic $ pfix #$ plam $ \self pred' xs ->
  pelimList
    (\x rest -> pif (pred' # x) (self # pred' # rest) (pconstant False))
    (pconstant True)
    xs

plistAny
  :: Term s ((PAsData PMultisigScript :--> PBool) :--> PBuiltinList (PAsData PMultisigScript) :--> PBool)
plistAny = phoistAcyclic $ pfix #$ plam $ \self pred' xs ->
  pelimList
    (\x rest -> pif (pred' # x) (pconstant True) (self # pred' # rest))
    (pconstant False)
    xs

plistCount
  :: Term
       s
       ((PAsData PMultisigScript :--> PBool) :--> PBuiltinList (PAsData PMultisigScript) :--> PInteger)
plistCount = phoistAcyclic $ pfix #$ plam $ \self pred' xs ->
  pelimList
    (\x rest -> pif (pred' # x) (1 + self # pred' # rest) (self # pred' # rest))
    0
    xs

ppairsHasKey
  :: Term
       s
       (PCredential :--> PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace)) :--> PBool)
ppairsHasKey = phoistAcyclic $ pfix #$ plam $ \self key xs ->
  pelimList
    (\x rest -> pif (pfromData (pfstBuiltin # x) #== key) (pconstant True) (self # key # rest))
    (pconstant False)
    xs

pmultisigSatisfied
  :: Term
       s
       ( PMultisigScript
           :--> PBuiltinList (PAsData PPubKeyHash)
           :--> PInterval PPosixTime
           :--> PBuiltinList (PBuiltinPair (PAsData PCredential) (PAsData PLovelace))
           :--> PBool
       )
pmultisigSatisfied = phoistAcyclic $ pfix #$ plam $ \self script signatories validRange withdrawals ->
  pmatch script $ \case
    PSignature{pkeyHash} ->
      plistElem # pfromData pkeyHash # signatories
    PAllOf{pscripts} ->
      plistAll
        # plam (\s -> self # pfromData s # signatories # validRange # withdrawals)
        # pfromData pscripts
    PAnyOf{pscripts} ->
      plistAny
        # plam (\s -> self # pfromData s # signatories # validRange # withdrawals)
        # pfromData pscripts
    PAtLeast{prequired, pscripts} ->
      pfromData prequired
        #<= ( plistCount
                # plam (\s -> self # pfromData s # signatories # validRange # withdrawals)
                # pfromData pscripts
            )
    PBefore{ptime} ->
      pmatch validRange $ \(PInterval _ ub) ->
        pmatch ub $ \(PUpperBound ext inclusive) ->
          pmatch ext $ \case
            PFinite hi ->
              pif
                (pfromData inclusive)
                (pfromData (punsafeCoerce @(PAsData PInteger) hi) #<= pfromData ptime)
                (pfromData (punsafeCoerce @(PAsData PInteger) hi) #< pfromData ptime)
            _ -> pconstant False
    PAfter{ptime} ->
      pmatch validRange $ \(PInterval lb _) ->
        pmatch lb $ \(PLowerBound ext inclusive) ->
          pmatch ext $ \case
            PFinite lo ->
              pif
                (pfromData inclusive)
                (pfromData ptime #<= pfromData (punsafeCoerce @(PAsData PInteger) lo))
                (pfromData ptime #< pfromData (punsafeCoerce @(PAsData PInteger) lo))
            _ -> pconstant False
    PScriptWit{pscriptHash} ->
      let cred = pcon (PScriptCredential (pdata (punsafeCoerce @PScriptHash (pfromData pscriptHash))))
       in ppairsHasKey # cred # withdrawals

-- ============================================================================
-- 3. Utility functions
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

pgetOutputDatum :: Term s (PTxOut :--> PSettingsDatum)
pgetOutputDatum = phoistAcyclic $ plam $ \txOut ->
  pmatch txOut $ \(PTxOut{ptxOut'datum}) ->
    pmatch ptxOut'datum $ \case
      POutputDatum d ->
        pfromData $ punsafeCoerce @(PAsData PSettingsDatum) (pto d)
      _ -> perror

pvalueWithoutLovelace :: Term s (PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive)
pvalueWithoutLovelace = phoistAcyclic $ plam $ \v ->
  let entries = pto (pto v)
   in pelimList
        ( \first rest ->
            let cs = pfromData (pfstBuiltin # first)
             in pif
                  (cs #== pconstant (CurrencySymbol ""))
                  (punsafeCoerce @(PValue 'Sorted 'Positive) (pcon (PMap rest)))
                  v
        )
        v
        entries

plistHeadOutput :: Term s (PBuiltinList (PAsData PTxOut) :--> PTxOut)
plistHeadOutput = phoistAcyclic $ plam $ \outputs ->
  pelimList
    (\x _ -> pfromData x)
    perror
    outputs

pvalueIsZero :: Term s (PValue 'Sorted 'NonZero :--> PBool)
pvalueIsZero = phoistAcyclic $ plam $ \v ->
  let entries = pto (pto v)
   in pelimList (\_ _ -> pconstant False) (pconstant True) entries

plistAnyInput
  :: Term s ((PAsData PTxInInfo :--> PBool) :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
plistAnyInput = phoistAcyclic $ pfix #$ plam $ \self pred' xs ->
  pelimList
    (\x rest -> pif (pred' # x) (pconstant True) (self # pred' # rest))
    (pconstant False)
    xs

-- ============================================================================
-- 4. Settings Admin Update
-- ============================================================================

pcheckSettingsAdminUpdate
  :: Term
       s
       ( PSettingsDatum
           :--> PSettingsDatum
           :--> PTxInfo
           :--> PBool
       )
pcheckSettingsAdminUpdate = phoistAcyclic $ plam $ \inputDatum outputDatum txInfo ->
  pmatch inputDatum $ \(PSettingsDatum{psdSettingsAdmin, psdAuthorizedStakingKeys, psdTreasuryAddress, psdTreasuryAllowance}) ->
    pmatch outputDatum $
      \( PSettingsDatum
           { psdAuthorizedStakingKeys = outStakingKeys
           , psdTreasuryAddress = outTreasuryAddr
           , psdTreasuryAllowance = outTreasuryAllow
           }
         ) ->
          let signatories = pfromData $ pmatch txInfo $ \txI -> ptxInfo'signatories txI
              validRange = pmatch txInfo $ \txI -> ptxInfo'validRange txI
              withdrawals = pto $ pfromData $ pmatch txInfo $ \txI -> ptxInfo'wdrl txI
              signedByAdmin = pmultisigSatisfied # pfromData psdSettingsAdmin # signatories # validRange # withdrawals
              stakingKeysUnchanged = outStakingKeys #== psdAuthorizedStakingKeys
              treasuryAddrUnchanged = outTreasuryAddr #== psdTreasuryAddress
              treasuryAllowUnchanged = outTreasuryAllow #== psdTreasuryAllowance
           in signedByAdmin
                #&& stakingKeysUnchanged
                #&& treasuryAddrUnchanged
                #&& treasuryAllowUnchanged

-- ============================================================================
-- 5. Treasury Admin Update
-- ============================================================================

pcheckTreasuryAdminUpdate
  :: Term
       s
       ( PSettingsDatum
           :--> PSettingsDatum
           :--> PTxInfo
           :--> PBool
       )
pcheckTreasuryAdminUpdate = phoistAcyclic $ plam $ \inputDatum outputDatum txInfo ->
  pmatch inputDatum $
    \( PSettingsDatum
         { psdTreasuryAdmin
         , psdSettingsAdmin
         , psdMetadataAdmin
         , psdAuthorizedScoopers
         , psdBaseFee
         , psdSimpleFee
         , psdStrategyFee
         , psdPoolCreationFee
         , psdExtensions
         }
       ) ->
        pmatch outputDatum $
          \( PSettingsDatum
               { psdSettingsAdmin = outSettingsAdmin
               , psdMetadataAdmin = outMetadataAdmin
               , psdTreasuryAdmin = outTreasuryAdmin
               , psdAuthorizedScoopers = outScoopers
               , psdBaseFee = outBaseFee
               , psdSimpleFee = outSimpleFee
               , psdStrategyFee = outStrategyFee
               , psdPoolCreationFee = outPoolCreationFee
               , psdExtensions = outExtensions
               }
             ) ->
              let signatories = pfromData $ pmatch txInfo $ \txI -> ptxInfo'signatories txI
                  validRange = pmatch txInfo $ \txI -> ptxInfo'validRange txI
                  withdrawals = pto $ pfromData $ pmatch txInfo $ \txI -> ptxInfo'wdrl txI
                  signedByAdmin = pmultisigSatisfied # pfromData psdTreasuryAdmin # signatories # validRange # withdrawals
                  settingsAdminUnchanged = outSettingsAdmin #== psdSettingsAdmin
                  metadataAdminUnchanged = outMetadataAdmin #== psdMetadataAdmin
                  treasuryAdminUnchanged = outTreasuryAdmin #== psdTreasuryAdmin
                  scoopersUnchanged = outScoopers #== psdAuthorizedScoopers
                  baseFeeUnchanged = outBaseFee #== psdBaseFee
                  simpleFeeUnchanged = outSimpleFee #== psdSimpleFee
                  strategyFeeUnchanged = outStrategyFee #== psdStrategyFee
                  poolCreationFeeUnchanged = outPoolCreationFee #== psdPoolCreationFee
                  extensionsUnchanged = outExtensions #== psdExtensions
               in signedByAdmin
                    #&& settingsAdminUnchanged
                    #&& metadataAdminUnchanged
                    #&& treasuryAdminUnchanged
                    #&& scoopersUnchanged
                    #&& baseFeeUnchanged
                    #&& simpleFeeUnchanged
                    #&& strategyFeeUnchanged
                    #&& poolCreationFeeUnchanged
                    #&& extensionsUnchanged

-- ============================================================================
-- 6. Main validator
-- ============================================================================

psettingsValidator :: Term s (PScriptContext :--> PSettingsDatum :--> PSettingsRedeemer :--> PUnit)
psettingsValidator = phoistAcyclic $ plam $ \ctx inputDatum redeemer ->
  pmatch ctx $ \(PScriptContext{pscriptContext'txInfo}) ->
    let txInfo = pscriptContext'txInfo
        ownInput = pfindOwnInput # ctx
        ownAddress = pmatch ownInput $ \(PTxInInfo{ptxInInfo'resolved}) ->
          pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'address}) -> ptxOut'address
        ownInputValue = pmatch ownInput $ \(PTxInInfo{ptxInInfo'resolved}) ->
          pmatch ptxInInfo'resolved $ \(PTxOut{ptxOut'value}) -> pfromData ptxOut'value
        outputs = pfromData $ pmatch txInfo $ \txI -> ptxInfo'outputs txI
        ownOutput = plistHeadOutput # outputs
        ownOutputAddress = pmatch ownOutput $ \(PTxOut{ptxOut'address}) -> ptxOut'address
        ownOutputValue = pmatch ownOutput $ \(PTxOut{ptxOut'value}) -> pfromData ptxOut'value
        outputDatum = pgetOutputDatum # ownOutput
        valueNotChanged = pvalueWithoutLovelace # ownOutputValue #== pvalueWithoutLovelace # ownInputValue
        noMint =
          pvalueIsZero # pfromData (pmatch txInfo $ \txI -> ptxInfo'mint txI)
     in pcheck $
          ownOutputAddress
            #== ownAddress
            #&& valueNotChanged
            #&& noMint
            #&& pmatch
              redeemer
              ( \case
                  PSettingsAdminUpdate ->
                    pcheckSettingsAdminUpdate # inputDatum # outputDatum # txInfo
                  PTreasuryAdminUpdate ->
                    pcheckTreasuryAdminUpdate # inputDatum # outputDatum # txInfo
              )

-- ============================================================================
-- 7. Mint validator
-- ============================================================================

pmintsExactlyOneToken
  :: Term s (PCurrencySymbol :--> PTokenName :--> PValue 'Sorted 'NonZero :--> PBool)
pmintsExactlyOneToken = phoistAcyclic $ plam $ \expectedCs expectedTn mintValue ->
  let mintEntries = pto (pto mintValue)
   in pelimList
        ( \first rest ->
            pelimList
              (\_ _ -> pconstant False)
              ( plet (pfromData (pfstBuiltin # first)) $ \cs ->
                  plet (pto (pfromData (psndBuiltin # first))) $ \toks ->
                    cs
                      #== expectedCs
                      #&& pelimList
                        ( \tk tkRest ->
                            pelimList
                              (\_ _ -> pconstant False)
                              ( pfromData (pfstBuiltin # tk)
                                  #== expectedTn
                                  #&& pfromData (psndBuiltin # tk)
                                  #== (1 :: Term _ PInteger)
                              )
                              tkRest
                        )
                        (pconstant False)
                        toks
              )
              rest
        )
        (pconstant False)
        mintEntries

pfindSettingsOutput :: Term s (PCurrencySymbol :--> PBuiltinList (PAsData PTxOut) :--> PBool)
pfindSettingsOutput = phoistAcyclic $ pfix #$ plam $ \self ownPolicyId outs ->
  pelimList
    ( \out rest ->
        pmatch (pfromData out) $ \(PTxOut{ptxOut'address, ptxOut'datum}) ->
          pmatch ptxOut'address $ \(PAddress{paddress'credential}) ->
            pmatch paddress'credential $ \case
              PScriptCredential sh ->
                pif
                  (pfromData sh #== punsafeCoerce @PScriptHash ownPolicyId)
                  ( pmatch ptxOut'datum $ \case
                      POutputDatum d ->
                        plet (punsafeCoerce @(PAsData PSettingsDatum) (pto d)) $ \_ ->
                          pconstant True
                      _ -> pconstant False
                  )
                  (self # ownPolicyId # rest)
              _ -> self # ownPolicyId # rest
    )
    (pconstant False)
    outs

psettingsMintValidator :: Term s (PTxOutRef :--> PCurrencySymbol :--> PTxInfo :--> PUnit)
psettingsMintValidator = phoistAcyclic $ plam $ \bootUtxo ownPolicyId txInfo ->
  let mintsExactlyOne =
        pmintsExactlyOneToken
          # ownPolicyId
          # pconstant (TokenName "settings")
          # pfromData (pmatch txInfo $ \txI -> ptxInfo'mint txI)
      spendsBootUtxo =
        plistAnyInput
          # plam
            ( \inp ->
                pmatch (pfromData inp) $ \(PTxInInfo{ptxInInfo'outRef}) ->
                  ptxInInfo'outRef #== bootUtxo
            )
          # pfromData (pmatch txInfo $ \txI -> ptxInfo'inputs txI)
      paysToSettingsScript =
        pfindSettingsOutput # ownPolicyId # pfromData (pmatch txInfo $ \txI -> ptxInfo'outputs txI)
   in pcheck $
        mintsExactlyOne
          #&& spendsBootUtxo
          #&& paysToSettingsScript

-- ============================================================================
-- 8. Entry point
-- ============================================================================

mkSettingsValidator :: Term s (PTxOutRef :--> PScriptContext :--> PUnit)
mkSettingsValidator = plam $ \bootUtxo ctx ->
  pmatch ctx $ \(PScriptContext{pscriptContext'redeemer, pscriptContext'scriptInfo}) ->
    pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum ->
        pmatch mDatum $ \case
          PDJust datumRaw ->
            let datum = pfromData $ punsafeCoerce @(PAsData PSettingsDatum) (pto (pfromData datumRaw))
                redeemer = pfromData $ punsafeCoerce @(PAsData PSettingsRedeemer) (pto pscriptContext'redeemer)
             in psettingsValidator # ctx # datum # redeemer
          PDNothing -> perror
      PMintingScript ownPolicyId ->
        pmatch ctx $ \(PScriptContext{pscriptContext'txInfo}) ->
          psettingsMintValidator # bootUtxo # pfromData ownPolicyId # pscriptContext'txInfo
      _ -> perror
