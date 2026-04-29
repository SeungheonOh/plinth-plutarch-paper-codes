{-# LANGUAGE OverloadedStrings #-}

module ProgrammableTokens.Test.ProgrammableLogicGlobal (
  mkGlobalTests,
) where

import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.Word (Word8)
import Plutarch.Builtin.Integer (pconstantInteger)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Internal.Term (Config (NoTracing), Script, Term, compile)
import Plutarch.LedgerApi.V3 qualified as PlutarchV3
import Plutarch.Prelude (pconstant, perror, pfromData, pif, plam, pmatch)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3
import PlutusTx qualified
import ProgrammableTokens.Test.ScriptContext.Builder (
  ScriptContextBuilder,
  buildBalancedScriptContext,
  buildScriptContext,
  mkAdaValue,
  withAddress,
  withInlineDatum,
  withInput,
  withMint,
  withOutRef,
  withOutput,
  withReferenceInput,
  withReferenceScript,
  withRewardingScript,
  withScriptInput,
  withSigner,
  withTxOutAddress,
  withTxOutInlineDatum,
  withTxOutReferenceScript,
  withTxOutValue,
  withValue,
  withWithdrawal,
 )
import SmartTokens.Contracts.ProgrammableLogicBase (
  mkProgrammableLogicGlobal,
  outputsContainExpectedValueAtCred,
  pscriptContextTxInfo,
 )
import SmartTokens.Types.Constants (protocolParamsToken)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (DirectorySetNode))
import SmartTokens.Types.ProgrammableLogicGlobal (
  ProgrammableLogicGlobalRedeemer (TransferAct),
  mkSeizeActRedeemerFromRelativeInputIdxs,
 )
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (ProgrammableLogicGlobalParams))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)

mkGlobalTests :: String -> Script -> TestTree
mkGlobalTests name script =
  let succeedsWith ctx =
        let (res, _budget, _logs) = evalScript (applyArguments script [PlutusTx.toData protocolParamsCS, PlutusTx.toData ctx])
         in isRight res
      failsWith ctx = not (succeedsWith ctx)
      assertSucceeds ctx =
        let (res, _budget, logs) = evalScript (applyArguments script [PlutusTx.toData protocolParamsCS, PlutusTx.toData ctx])
         in assertBool ("expected successful evaluation, got: " ++ show res ++ " logs: " ++ show logs) (isRight res)
      assertFails ctx = assertBool "expected script failure" (failsWith ctx)
   in testGroup
        name
        [ testCase "unit_seizeAct_complete_indices_succeeds" $ assertSucceeds (mkGlobalSeizeCtx 3 [0, 0, 0])
        , testCase "unit_seizeAct_pubkey_index_rejected" $ assertFails (mkGlobalSeizeCtxWithLeadingPubKey 1 [0])
        , testCase "unit_seizeAct_omitted_index_rejected" $ assertFails (mkGlobalSeizeCtx 3 [0, 0])
        , testCase "unit_seizeAct_datum_mismatch_rejected" $ assertFails mkGlobalSeizeDatumMismatchCtx
        , testCase "unit_seizeAct_reference_script_mismatch_rejected" $ assertFails mkGlobalSeizeReferenceScriptMismatchCtx
        , testCase "unit_seizeAct_burn_offsets_delta_succeeds" $ assertSucceeds mkGlobalSeizeBurnCtx
        , testCase "unit_seizeAct_mint_with_containment_succeeds" $ assertSucceeds mkGlobalSeizeMintContainedCtx
        , testCase "unit_seizeAct_mint_smuggle_rejected" $ assertFails mkGlobalSeizeMintEscapeCtx
        , testCase "unit_transferAct_burn_with_mint_proof_succeeds" $ assertSucceeds (mkGlobalTransferMintCtx (TransferAct [1] [1]) (-1) 0)
        , testCase "unit_transferAct_burn_without_mint_proof_rejected" $ assertFails (mkGlobalTransferMintCtx (TransferAct [1] []) (-1) 0)
        , testCase "unit_transferAct_escape_to_pubkey_rejected" $ assertFails mkGlobalTransferEscapeCtx
        , testCase "unit_transferAct_mint_smuggle_rejected" $ assertFails (mkGlobalTransferMintCtx (TransferAct [1] [1]) 1 1)
        , testCase "unit_transferAct_mint_with_proof_and_containment_succeeds" $ assertSucceeds (mkGlobalTransferMintCtx (TransferAct [1] [1]) 1 2)
        , testCase "unit_transferAct_mint_without_mint_proof_rejected" $ assertFails (mkGlobalTransferMintCtx (TransferAct [1] []) 1 2)
        , testCase "unit_seizeAct_escape_to_pubkey_rejected" $ assertFails mkGlobalSeizeDirectEscapeCtx
        , testProperty "prop_seizeAct_complete_indices_succeeds" $
            QC.forAll (QC.chooseInt (1, 12)) $ \nInt ->
              let n = fromIntegral nInt
               in succeedsWith (mkGlobalSeizeCtx n (replicate nInt 0)) QC.=== True
        , testProperty "prop_seizeAct_omitted_index_rejected" $
            QC.forAll (QC.chooseInt (2, 12)) $ \nInt ->
              let n = fromIntegral nInt
               in failsWith (mkGlobalSeizeCtx n (replicate (nInt - 1) 0)) QC.=== True
        , testCase "unit_outputsContain_single_asset_split_across_prog_outputs_succeeds" $
            assertOutputsContainSucceeds
              progLogicBaseCred
              [ txOutAt progWalletA (mkValue [(programmableTransferCS, TokenName "0c", 2)])
              , txOutAt progWalletB (mkValue [(programmableTransferCS, TokenName "0c", 3)])
              , txOutAt (pubKeyAddress signerPkh) (mkValue [(programmableTransferCS, TokenName "0c", 50)])
              ]
              (mkValue [(programmableTransferCS, TokenName "0c", 5)])
        , testCase "unit_outputsContain_single_asset_pubkey_output_ignored" $
            assertOutputsContainFails
              progLogicBaseCred
              [ txOutAt progWalletA (mkValue [(programmableTransferCS, TokenName "0c", 4)])
              , txOutAt (pubKeyAddress signerPkh) (mkValue [(programmableTransferCS, TokenName "0c", 100)])
              ]
              (mkValue [(programmableTransferCS, TokenName "0c", 5)])
        , testCase "unit_outputsContain_multi_asset_succeeds" $
            assertOutputsContainSucceeds
              progLogicBaseCred
              [ txOutAt progWalletA (mkValue [(programmableTransferCS, TokenName "0c", 2), (programmableTransferCS, TokenName "0d", 1)])
              , txOutAt progWalletB (mkValue [(programmableTransferCS, TokenName "0c", 3), (programmableTransferCS, TokenName "0d", 4)])
              , txOutAt (pubKeyAddress signerPkh) (mkValue [(programmableTransferCS, TokenName "0d", 100)])
              ]
              (mkValue [(programmableTransferCS, TokenName "0c", 5), (programmableTransferCS, TokenName "0d", 5)])
        , testCase "unit_outputsContain_multi_asset_shortfall_rejected" $
            assertOutputsContainFails
              progLogicBaseCred
              [ txOutAt progWalletA (mkValue [(programmableTransferCS, TokenName "0c", 2), (programmableTransferCS, TokenName "0d", 1)])
              , txOutAt progWalletB (mkValue [(programmableTransferCS, TokenName "0c", 3), (programmableTransferCS, TokenName "0d", 3)])
              , txOutAt (pubKeyAddress signerPkh) (mkValue [(programmableTransferCS, TokenName "0d", 100)])
              ]
              (mkValue [(programmableTransferCS, TokenName "0c", 5), (programmableTransferCS, TokenName "0d", 5)])
        ]

assertOutputsContainSucceeds :: Credential -> [TxOut] -> Value -> Assertion
assertOutputsContainSucceeds cred outputs expected =
  let ctx =
        buildScriptContext $
          mconcat
            [ withOutput
                ( withTxOutAddress (txOutAddress txOut)
                    <> withTxOutValue (txOutValue txOut)
                )
            | txOut <- outputs
            ]
      (res, _budget, _logs) =
        evalScript
          ( applyArguments
              (mkOutputsContainScript cred expected)
              [PlutusTx.toData ctx]
          )
   in assertBool "expected outputsContain helper to succeed" (isRight res)

assertOutputsContainFails :: Credential -> [TxOut] -> Value -> Assertion
assertOutputsContainFails cred outputs expected =
  let ctx =
        buildScriptContext $
          mconcat
            [ withOutput
                ( withTxOutAddress (txOutAddress txOut)
                    <> withTxOutValue (txOutValue txOut)
                )
            | txOut <- outputs
            ]
      (res, _budget, _logs) =
        evalScript
          ( applyArguments
              (mkOutputsContainScript cred expected)
              [PlutusTx.toData ctx]
          )
   in assertBool "expected outputsContain helper to fail" (isLeft res)

compileNoTracing :: (forall s. Term s a) -> Script
compileNoTracing term =
  either (error . ("compile failed: " <>) . show) id (compile NoTracing term)

mkOutputsContainScript :: Credential -> Value -> Script
mkOutputsContainScript cred expectedValue =
  compileNoTracing $
    plam $ \ctx ->
      pmatch (pscriptContextTxInfo ctx) $ \txInfo ->
        let expectedValueTerm =
              punsafeCoerce $
                pconstant @(PlutarchV3.PValue 'PlutarchV3.Unsorted 'PlutarchV3.NoGuarantees) expectedValue
         in pif
              ( outputsContainExpectedValueAtCred
                  (pconstant cred)
                  (pfromData $ PlutarchV3.ptxInfo'outputs txInfo)
                  expectedValueTerm
              )
              (pconstantInteger 0)
              perror

mkValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
mkValue = foldMap (\(cs, tn, amount) -> assetClassValue (assetClass cs tn) amount)

bs28 :: Word8 -> BuiltinByteString
bs28 w = PV1.toBuiltin (BS.replicate 28 w)

protocolParamsCS :: CurrencySymbol
protocolParamsCS = CurrencySymbol (bs28 0x10)

directoryNodeCS :: CurrencySymbol
directoryNodeCS = CurrencySymbol (bs28 0x11)

progLogicBaseHash :: ScriptHash
progLogicBaseHash = ScriptHash (bs28 0x12)

globalScriptHash :: ScriptHash
globalScriptHash = ScriptHash (bs28 0x13)

globalCred :: Credential
globalCred = ScriptCredential globalScriptHash

progLogicBaseCred :: Credential
progLogicBaseCred = ScriptCredential progLogicBaseHash

issuerLogicHash :: ScriptHash
issuerLogicHash = ScriptHash (bs28 0x14)

issuerCred :: Credential
issuerCred = ScriptCredential issuerLogicHash

programmableTransferCS :: CurrencySymbol
programmableTransferCS = CurrencySymbol (bs28 0x1b)

signerPkh :: PubKeyHash
signerPkh = PubKeyHash (bs28 0x01)

paramRef :: TxOutRef
paramRef = TxOutRef "aa00" 0

dirNodeRef :: TxOutRef
dirNodeRef = TxOutRef "bb00" 0

scriptAddressWithSignerStake :: ScriptHash -> PubKeyHash -> Address
scriptAddressWithSignerStake sh pkh =
  Address (ScriptCredential sh) (Just (StakingHash (PubKeyCredential pkh)))

pubKeyAddress :: PubKeyHash -> Address
pubKeyAddress pkh = Address (PubKeyCredential pkh) Nothing

txOutAt :: Address -> Value -> TxOut
txOutAt addr assets =
  TxOut
    { txOutAddress = addr
    , txOutValue = mkAdaValue 3_000_000 <> assets
    , txOutDatum = NoOutputDatum
    , txOutReferenceScript = Nothing
    }

withRefInputDatumValue :: TxOutRef -> Address -> Value -> BuiltinData -> ScriptContextBuilder
withRefInputDatumValue ref addr value dat =
  withReferenceInput
    ( withOutRef ref
        <> withAddress addr
        <> withValue value
        <> withInlineDatum dat
    )

protocolParamsDatum :: ProgrammableLogicGlobalParams
protocolParamsDatum =
  ProgrammableLogicGlobalParams directoryNodeCS (ScriptCredential progLogicBaseHash)

directoryProgrammableNode :: DirectorySetNode
directoryProgrammableNode =
  DirectorySetNode
    programmableTransferCS
    (CurrencySymbol (bs28 0xff))
    (ScriptCredential (ScriptHash (bs28 0x15)))
    issuerCred
    (CurrencySymbol "")

transferLogicHash :: ScriptHash
transferLogicHash = ScriptHash (bs28 0x15)

transferCred :: Credential
transferCred = ScriptCredential transferLogicHash

seizeInputAddr :: Address
seizeInputAddr = scriptAddressWithSignerStake progLogicBaseHash signerPkh

progWalletA :: Address
progWalletA = scriptAddressWithSignerStake progLogicBaseHash signerPkh

progWalletB :: Address
progWalletB = scriptAddressWithSignerStake progLogicBaseHash (PubKeyHash (bs28 0x02))

metadataDatumA :: BuiltinData
metadataDatumA = PlutusTx.toBuiltinData (1 :: Integer)

metadataDatumB :: BuiltinData
metadataDatumB = PlutusTx.toBuiltinData (2 :: Integer)

metadataRefScriptA :: ScriptHash
metadataRefScriptA = ScriptHash (bs28 0x31)

metadataRefScriptB :: ScriptHash
metadataRefScriptB = ScriptHash (bs28 0x32)

seizeInputValue :: Value
seizeInputValue =
  mkAdaValue 3_000_000
    <> mkValue [(programmableTransferCS, TokenName "0c", 1)]

seizeInputBuilder :: Integer -> ScriptContextBuilder
seizeInputBuilder idx =
  withScriptInput
    (PlutusTx.toBuiltinData ())
    ( withOutRef (TxOutRef "5e12" idx)
        <> withAddress seizeInputAddr
        <> withValue seizeInputValue
    )

seizeCorrespondingOutputBuilder :: ScriptContextBuilder
seizeCorrespondingOutputBuilder =
  withOutput
    ( withTxOutAddress seizeInputAddr
        <> withTxOutValue seizeInputValue
    )

seizeInputWithMetadataBuilder :: BuiltinData -> ScriptHash -> ScriptContextBuilder
seizeInputWithMetadataBuilder datum refScript =
  withScriptInput
    (PlutusTx.toBuiltinData ())
    ( withOutRef (TxOutRef "5e99" 0)
        <> withAddress seizeInputAddr
        <> withValue seizeInputValue
        <> withInlineDatum datum
        <> withReferenceScript refScript
    )

seizeCorrespondingOutputWithMetadataBuilder :: BuiltinData -> ScriptHash -> ScriptContextBuilder
seizeCorrespondingOutputWithMetadataBuilder datum refScript =
  withOutput
    ( withTxOutAddress seizeInputAddr
        <> withTxOutValue seizeInputValue
        <> withTxOutInlineDatum datum
        <> withTxOutReferenceScript refScript
    )

transferInputRef :: TxOutRef
transferInputRef = TxOutRef "7a00" 0

mkGlobalTransferMintCtx :: ProgrammableLogicGlobalRedeemer -> Integer -> Integer -> ScriptContext
mkGlobalTransferMintCtx globalRedeemer mintedQty transferOutputQty =
  buildBalancedScriptContext
    ( withRewardingScript
        (PlutusTx.toBuiltinData globalRedeemer)
        globalCred
        0
        <> withSigner signerPkh
        <> withWithdrawal transferCred 0
        <> withScriptInput
          (PlutusTx.toBuiltinData ())
          ( withOutRef transferInputRef
              <> withAddress seizeInputAddr
              <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 1)])
          )
        <> withOutput
          ( withTxOutAddress seizeInputAddr
              <> withTxOutValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", transferOutputQty)])
          )
        <> withMint (mkValue [(programmableTransferCS, TokenName "0c", mintedQty)]) (PlutusTx.toBuiltinData ())
        <> withRefInputDatumValue
          paramRef
          (pubKeyAddress signerPkh)
          (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
          (PlutusTx.toBuiltinData protocolParamsDatum)
        <> withRefInputDatumValue
          dirNodeRef
          (pubKeyAddress signerPkh)
          (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
          (PlutusTx.toBuiltinData directoryProgrammableNode)
    )

mkGlobalTransferEscapeCtx :: ScriptContext
mkGlobalTransferEscapeCtx =
  buildBalancedScriptContext
    ( withRewardingScript
        (PlutusTx.toBuiltinData $ TransferAct [1] [])
        globalCred
        0
        <> withSigner signerPkh
        <> withWithdrawal transferCred 0
        <> withScriptInput
          (PlutusTx.toBuiltinData ())
          ( withOutRef transferInputRef
              <> withAddress seizeInputAddr
              <> withValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 1)])
          )
        <> withOutput
          ( withTxOutAddress (pubKeyAddress signerPkh)
              <> withTxOutValue (mkAdaValue 10_000_000 <> mkValue [(programmableTransferCS, TokenName "0c", 1)])
          )
        <> withRefInputDatumValue
          paramRef
          (pubKeyAddress signerPkh)
          (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
          (PlutusTx.toBuiltinData protocolParamsDatum)
        <> withRefInputDatumValue
          dirNodeRef
          (pubKeyAddress signerPkh)
          (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
          (PlutusTx.toBuiltinData directoryProgrammableNode)
    )

mkGlobalSeizeCtx :: Integer -> [Integer] -> ScriptContext
mkGlobalSeizeCtx inputCount providedIdxs =
  let expectedIdxs = [0 .. (inputCount - 1)]
      seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 providedIdxs 0
      seizeInputsBuilder = mconcat (map seizeInputBuilder expectedIdxs)
      correspondingOutputsBuilder = mconcat (replicate (length providedIdxs) seizeCorrespondingOutputBuilder)
   in buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData seizeRedeemer)
            globalCred
            0
            <> withWithdrawal issuerCred 0
            <> seizeInputsBuilder
            <> correspondingOutputsBuilder
            <> withRefInputDatumValue
              paramRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
              (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
              dirNodeRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
              (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeCtxWithLeadingPubKey :: Integer -> [Integer] -> ScriptContext
mkGlobalSeizeCtxWithLeadingPubKey inputCount providedIdxs =
  let expectedIdxs = [0 .. (inputCount - 1)]
      seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 providedIdxs 0
      leadingPubKeyInput =
        withInput
          ( withOutRef (TxOutRef "0000" 0)
              <> withAddress (pubKeyAddress signerPkh)
              <> withValue (mkAdaValue 2_000_000)
          )
      seizeInputsBuilder = mconcat (map seizeInputBuilder expectedIdxs)
      correspondingOutputsBuilder = mconcat (replicate (length providedIdxs) seizeCorrespondingOutputBuilder)
   in buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData seizeRedeemer)
            globalCred
            0
            <> withWithdrawal issuerCred 0
            <> leadingPubKeyInput
            <> seizeInputsBuilder
            <> correspondingOutputsBuilder
            <> withRefInputDatumValue
              paramRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
              (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
              dirNodeRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
              (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeDatumMismatchCtx :: ScriptContext
mkGlobalSeizeDatumMismatchCtx =
  let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0
   in buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData seizeRedeemer)
            globalCred
            0
            <> withWithdrawal issuerCred 0
            <> seizeInputWithMetadataBuilder metadataDatumA metadataRefScriptA
            <> seizeCorrespondingOutputWithMetadataBuilder metadataDatumB metadataRefScriptA
            <> withRefInputDatumValue
              paramRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
              (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
              dirNodeRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
              (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeReferenceScriptMismatchCtx :: ScriptContext
mkGlobalSeizeReferenceScriptMismatchCtx =
  let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0
   in buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData seizeRedeemer)
            globalCred
            0
            <> withWithdrawal issuerCred 0
            <> seizeInputWithMetadataBuilder metadataDatumA metadataRefScriptA
            <> seizeCorrespondingOutputWithMetadataBuilder metadataDatumA metadataRefScriptB
            <> withRefInputDatumValue
              paramRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
              (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
              dirNodeRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
              (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeBurnCtx :: ScriptContext
mkGlobalSeizeBurnCtx =
  let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0
   in buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData seizeRedeemer)
            globalCred
            0
            <> withWithdrawal issuerCred 0
            <> seizeInputBuilder 0
            -- corresponding output removes 1 programmable token from the input
            -- and tx mint burns exactly 1, so net required residual is zero.
            <> withOutput
              ( withTxOutAddress seizeInputAddr
                  <> withTxOutValue (mkAdaValue 3_000_000)
              )
            <> withMint (mkValue [(programmableTransferCS, TokenName "0c", -1)]) (PlutusTx.toBuiltinData ())
            <> withRefInputDatumValue
              paramRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
              (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
              dirNodeRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
              (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeMintContainedCtx :: ScriptContext
mkGlobalSeizeMintContainedCtx =
  let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0
   in buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData seizeRedeemer)
            globalCred
            0
            <> withWithdrawal issuerCred 0
            <> seizeInputBuilder 0
            -- output ordering is reversed by the builder; this yields
            -- [corresponding, residual] in the final tx outputs.
            <> withOutput
              ( withTxOutAddress seizeInputAddr
                  <> withTxOutValue (mkValue [(programmableTransferCS, TokenName "0c", 1)])
              )
            <> withOutput
              ( withTxOutAddress seizeInputAddr
                  <> withTxOutValue seizeInputValue
              )
            <> withMint (mkValue [(programmableTransferCS, TokenName "0c", 1)]) (PlutusTx.toBuiltinData ())
            <> withRefInputDatumValue
              paramRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
              (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
              dirNodeRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
              (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeMintEscapeCtx :: ScriptContext
mkGlobalSeizeMintEscapeCtx =
  let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 1
   in buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData seizeRedeemer)
            globalCred
            0
            <> withWithdrawal issuerCred 0
            <> seizeInputBuilder 0
            -- output ordering is reversed by the builder; this yields
            -- [pubkey token output, corresponding] in the final tx outputs.
            <> withOutput
              ( withTxOutAddress seizeInputAddr
                  <> withTxOutValue seizeInputValue
              )
            <> withOutput
              ( withTxOutAddress (pubKeyAddress signerPkh)
                  <> withTxOutValue (mkValue [(programmableTransferCS, TokenName "0c", 1)])
              )
            <> withMint (mkValue [(programmableTransferCS, TokenName "0c", 1)]) (PlutusTx.toBuiltinData ())
            <> withRefInputDatumValue
              paramRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
              (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
              dirNodeRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
              (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeDirectEscapeCtx :: ScriptContext
mkGlobalSeizeDirectEscapeCtx =
  let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0
   in buildBalancedScriptContext
        ( withRewardingScript
            (PlutusTx.toBuiltinData seizeRedeemer)
            globalCred
            0
            <> withWithdrawal issuerCred 0
            <> seizeInputBuilder 0
            <> withOutput
              ( withTxOutAddress (pubKeyAddress signerPkh)
                  <> withTxOutValue seizeInputValue
              )
            <> withRefInputDatumValue
              paramRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)])
              (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue
              dirNodeRef
              (pubKeyAddress signerPkh)
              (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)])
              (PlutusTx.toBuiltinData directoryProgrammableNode)
        )
