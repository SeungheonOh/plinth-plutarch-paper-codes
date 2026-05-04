{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Main where

import Constitution.Contracts.ConstitutionSorted (mkConstitutionValidator)
import Constitution.Contracts.ConstitutionSortedPlinth (plinthConstitutionScript)
import Constitution.Test.ConstitutionSorted qualified as ConstitutionSorted
import Constitution.Types.ConstitutionConfig (ParamValue)
import Crowdfund.Contracts.Crowdfund (mkCrowdfundValidator)
import Crowdfund.Contracts.CrowdfundPlinth (plinthCrowdfundScript)
import Crowdfund.Test.Crowdfund qualified as Crowdfund
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.Word (Word8)
import Hydra.Contracts.Head (mkHeadValidator)
import Hydra.Contracts.HeadPlinth (plinthHeadScript)
import Hydra.Test.Head qualified as HydraHead
import Plutarch.Builtin.Integer (pconstantInteger)
import Plutarch.Evaluate (applyArguments, evalScript)
import Plutarch.Internal.Evaluate (evalScript')
import Plutarch.Internal.Term (Config (NoTracing), Script, Term, compile)
import Plutarch.LedgerApi.V3 qualified as PlutarchV3
import Plutarch.Prelude (pconstant, perror, pfromData, pif, plam, pmatch)
import Plutarch.Script (Script)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (ExBudget))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue)
import PlutusLedgerApi.V3
import PlutusTx qualified
import ProgrammableTokens.Test.ScriptContext.Builder
import Settings.Contracts.Settings (mkSettingsValidator)
import Settings.Contracts.SettingsPlinth (plinthSettingsScript)
import Settings.Test.Settings qualified as Settings
import SmartTokens.Contracts.ProgrammableLogicBase
import SmartTokens.Contracts.ProgrammableLogicBasePlinth (plinthProgrammableLogicGlobalScript)
import SmartTokens.Types.Constants (protocolParamsToken)
import SmartTokens.Types.PTokenDirectory (DirectorySetNode (DirectorySetNode))
import SmartTokens.Types.ProgrammableLogicGlobal
import SmartTokens.Types.ProtocolParams (ProgrammableLogicGlobalParams (ProgrammableLogicGlobalParams))
import Text.Printf (printf)
import Vesting.Contracts.Vesting (mkVestingValidator)
import Vesting.Contracts.VestingPlinth (plinthVestingScript)
import Vesting.Test.Vesting qualified as Vesting

plutarchHeadScript :: Script
plutarchHeadScript = compileNoTracing mkHeadValidator

plutarchConstitutionScript :: Script
plutarchConstitutionScript = compileNoTracing mkConstitutionValidator

plutarchCrowdfundScript :: Script
plutarchCrowdfundScript = compileNoTracing mkCrowdfundValidator

plutarchSettingsScript :: Script
plutarchSettingsScript = compileNoTracing mkSettingsValidator

plutarchVestingScript :: Script
plutarchVestingScript = compileNoTracing mkVestingValidator

main :: IO ()
main = do
  putStrLn ""
  putStrLn "CIP-143 ProgrammableLogicGlobal -- Execution Cost Comparison"
  putStrLn (replicate 140 '=')
  printHeader
  mapM_ runComparison scenarios
  putStrLn (replicate 140 '=')

  putStrLn ""
  putStrLn "Hydra Head Validator -- Execution Cost Comparison"
  putStrLn (replicate 140 '=')
  printHeader
  mapM_ (runHeadComparison plutarchHeadScript plinthHeadScript) HydraHead.headBenchScenarios
  putStrLn (replicate 140 '=')

  putStrLn ""
  putStrLn "Constitution Sorted Validator -- Execution Cost Comparison"
  putStrLn (replicate 140 '=')
  printHeader
  mapM_ (runConstitutionComparison plutarchConstitutionScript plinthConstitutionScript) ConstitutionSorted.constitutionBenchScenarios
  putStrLn (replicate 140 '=')

  putStrLn ""
  putStrLn "Crowdfund Validator -- Execution Cost Comparison"
  putStrLn (replicate 140 '=')
  printHeader
  mapM_ (runCrowdfundComparison plutarchCrowdfundScript plinthCrowdfundScript) Crowdfund.crowdfundBenchScenarios
  putStrLn (replicate 140 '=')

  putStrLn ""
  putStrLn "Settings Validator -- Execution Cost Comparison"
  putStrLn (replicate 140 '=')
  printHeader
  mapM_ (runSettingsComparison plutarchSettingsScript plinthSettingsScript) Settings.settingsBenchScenarios
  putStrLn (replicate 140 '=')

  putStrLn ""
  putStrLn "Vesting Validator -- Execution Cost Comparison"
  putStrLn (replicate 140 '=')
  printHeader
  mapM_ (runVestingComparison plutarchVestingScript plinthVestingScript) Vesting.vestingBenchScenarios
  putStrLn (replicate 140 '=')

printHeader :: IO ()
printHeader = do
  printf
    "%-40s | %-12s %-12s %-4s | %-12s %-12s %-4s | %-10s %-10s\n"
    ("Scenario" :: String)
    ("CPU" :: String)
    ("Mem" :: String)
    ("" :: String)
    ("CPU" :: String)
    ("Mem" :: String)
    ("" :: String)
    ("CPU" :: String)
    ("Mem" :: String)
  printf
    "%-40s | %-12s %-12s %-4s | %-12s %-12s %-4s | %-10s %-10s\n"
    ("" :: String)
    ("Plutarch" :: String)
    ("" :: String)
    ("" :: String)
    ("Plinth" :: String)
    ("" :: String)
    ("" :: String)
    ("ratio" :: String)
    ("ratio" :: String)
  putStrLn (replicate 140 '-')

evalWith :: Script -> ScriptContext -> (Bool, String, String)
evalWith script ctx =
  let applied = applyArguments script [PlutusTx.toData protocolParamsCS, PlutusTx.toData ctx]
      (res, ExBudget (ExCPU cpu) (ExMemory mem), _logs) = evalScript applied
      ok = isRight res
   in (ok, show cpu, show mem)

ratioStr :: String -> String -> String
ratioStr a b = case (reads a, reads b) of
  ([(va, _)], [(vb, _)]) | va > (0 :: Double) -> printf "%.2fx" (vb / va)
  _ -> "N/A"

runComparison :: (String, ScriptContext) -> IO ()
runComparison (name, ctx) = do
  let (okP, cpuP, memP) = evalWith plutarchScript ctx
      (okT, cpuT, memT) = evalWith plinthScript ctx
      statusP = if okP then "OK" else "FAIL" :: String
      statusT = if okT then "OK" else "FAIL" :: String
      cpuR = ratioStr cpuP cpuT
      memR = ratioStr memP memT
  printf "%-40s | %12s %12s %-4s | %12s %12s %-4s | %-10s %-10s\n" name cpuP memP statusP cpuT memT statusT cpuR memR

evalHeadWith :: Script -> ScriptContext -> (Bool, String, String)
evalHeadWith script ctx =
  let applied = applyArguments script [PlutusTx.toData ctx]
      (res, ExBudget (ExCPU cpu) (ExMemory mem), _logs) = evalScript applied
      ok = isRight res
   in (ok, show cpu, show mem)

evalConstitutionWith :: Script -> [(Integer, ParamValue)] -> ScriptContext -> (Bool, String, String)
evalConstitutionWith script config ctx =
  let applied = applyArguments script [PlutusTx.toData config, PlutusTx.toData ctx]
      (res, ExBudget (ExCPU cpu) (ExMemory mem), _logs) = evalScript applied
      ok = isRight res
   in (ok, show cpu, show mem)

runConstitutionComparison :: Script -> Script -> (String, [(Integer, ParamValue)], ScriptContext) -> IO ()
runConstitutionComparison plutarchS plinthS (name, config, ctx) = do
  let (okP, cpuP, memP) = evalConstitutionWith plutarchS config ctx
      (okT, cpuT, memT) = evalConstitutionWith plinthS config ctx
      statusP = if okP then "OK" else "FAIL" :: String
      statusT = if okT then "OK" else "FAIL" :: String
      cpuR = ratioStr cpuP cpuT
      memR = ratioStr memP memT
  printf "%-40s | %12s %12s %-4s | %12s %12s %-4s | %-10s %-10s\n" name cpuP memP statusP cpuT memT statusT cpuR memR

runHeadComparison :: Script -> Script -> (String, ScriptContext) -> IO ()
runHeadComparison plutarchS plinthS (name, ctx) = do
  let (okP, cpuP, memP) = evalHeadWith plutarchS ctx
      (okT, cpuT, memT) = evalHeadWith plinthS ctx
      statusP = if okP then "OK" else "FAIL" :: String
      statusT = if okT then "OK" else "FAIL" :: String
      cpuR = ratioStr cpuP cpuT
      memR = ratioStr memP memT
  printf "%-40s | %12s %12s %-4s | %12s %12s %-4s | %-10s %-10s\n" name cpuP memP statusP cpuT memT statusT cpuR memR

runCrowdfundComparison :: Script -> Script -> (String, ScriptContext) -> IO ()
runCrowdfundComparison plutarchS plinthS (name, ctx) = do
  let (okP, cpuP, memP) = evalHeadWith plutarchS ctx
      (okT, cpuT, memT) = evalHeadWith plinthS ctx
      statusP = if okP then "OK" else "FAIL" :: String
      statusT = if okT then "OK" else "FAIL" :: String
      cpuR = ratioStr cpuP cpuT
      memR = ratioStr memP memT
  printf "%-40s | %12s %12s %-4s | %12s %12s %-4s | %-10s %-10s\n" name cpuP memP statusP cpuT memT statusT cpuR memR

settingsBootUtxoRef :: TxOutRef
settingsBootUtxoRef = Settings.bootUtxoRef

evalSettingsWith :: Script -> ScriptContext -> (Bool, String, String)
evalSettingsWith script ctx =
  let applied = applyArguments script [PlutusTx.toData settingsBootUtxoRef, PlutusTx.toData ctx]
      (res, ExBudget (ExCPU cpu) (ExMemory mem), _logs) = evalScript applied
      ok = isRight res
   in (ok, show cpu, show mem)

runSettingsComparison :: Script -> Script -> (String, ScriptContext) -> IO ()
runSettingsComparison plutarchS plinthS (name, ctx) = do
  let (okP, cpuP, memP) = evalSettingsWith plutarchS ctx
      (okT, cpuT, memT) = evalSettingsWith plinthS ctx
      statusP = if okP then "OK" else "FAIL" :: String
      statusT = if okT then "OK" else "FAIL" :: String
      cpuR = ratioStr cpuP cpuT
      memR = ratioStr memP memT
  printf "%-40s | %12s %12s %-4s | %12s %12s %-4s | %-10s %-10s\n" name cpuP memP statusP cpuT memT statusT cpuR memR

runVestingComparison :: Script -> Script -> (String, ScriptContext) -> IO ()
runVestingComparison plutarchS plinthS (name, ctx) = do
  let (okP, cpuP, memP) = evalHeadWith plutarchS ctx
      (okT, cpuT, memT) = evalHeadWith plinthS ctx
      statusP = if okP then "OK" else "FAIL" :: String
      statusT = if okT then "OK" else "FAIL" :: String
      cpuR = ratioStr cpuP cpuT
      memR = ratioStr memP memT
  printf "%-40s | %12s %12s %-4s | %12s %12s %-4s | %-10s %-10s\n" name cpuP memP statusP cpuT memT statusT cpuR memR

plutarchScript :: Script
plutarchScript = compileNoTracing mkProgrammableLogicGlobal

plinthScript :: Script
plinthScript = plinthProgrammableLogicGlobalScript

scenarios :: [(String, ScriptContext)]
scenarios =
  [ ("seize: 1 input, complete indices", mkGlobalSeizeCtx 1 [0])
  , ("seize: 3 inputs, complete indices", mkGlobalSeizeCtx 3 [0, 0, 0])
  , ("seize: 5 inputs, complete indices", mkGlobalSeizeCtx 5 [0, 0, 0, 0, 0])
  , ("seize: 10 inputs, complete indices", mkGlobalSeizeCtx 10 (replicate 10 0))
  , ("seize: pubkey index (reject)", mkGlobalSeizeCtxWithLeadingPubKey 1 [0])
  , ("seize: omitted index (reject)", mkGlobalSeizeCtx 3 [0, 0])
  , ("seize: datum mismatch (reject)", mkGlobalSeizeDatumMismatchCtx)
  , ("seize: ref script mismatch (reject)", mkGlobalSeizeReferenceScriptMismatchCtx)
  , ("seize: burn offsets delta", mkGlobalSeizeBurnCtx)
  , ("seize: mint contained", mkGlobalSeizeMintContainedCtx)
  , ("seize: mint smuggle (reject)", mkGlobalSeizeMintEscapeCtx)
  , ("seize: escape to pubkey (reject)", mkGlobalSeizeDirectEscapeCtx)
  , ("transfer: burn with mint proof", mkGlobalTransferMintCtx (TransferAct [1] [1]) (-1) 0)
  , ("transfer: burn no mint proof (reject)", mkGlobalTransferMintCtx (TransferAct [1] []) (-1) 0)
  , ("transfer: escape to pubkey (reject)", mkGlobalTransferEscapeCtx)
  , ("transfer: mint smuggle (reject)", mkGlobalTransferMintCtx (TransferAct [1] [1]) 1 1)
  , ("transfer: mint with proof+containment", mkGlobalTransferMintCtx (TransferAct [1] [1]) 1 2)
  , ("transfer: mint no proof (reject)", mkGlobalTransferMintCtx (TransferAct [1] []) 1 2)
  ]

compileNoTracing :: (forall s. Term s a) -> Script
compileNoTracing term =
  either (error . ("compile failed: " <>) . show) id (compile NoTracing term)

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

transferLogicHash :: ScriptHash
transferLogicHash = ScriptHash (bs28 0x15)

transferCred :: Credential
transferCred = ScriptCredential transferLogicHash

seizeInputAddr :: Address
seizeInputAddr = scriptAddressWithSignerStake progLogicBaseHash signerPkh

seizeInputValue :: Value
seizeInputValue =
  mkAdaValue 3_000_000
    <> mkValue [(programmableTransferCS, TokenName "0c", 1)]

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

metadataDatumA :: BuiltinData
metadataDatumA = PlutusTx.toBuiltinData (1 :: Integer)

metadataDatumB :: BuiltinData
metadataDatumB = PlutusTx.toBuiltinData (2 :: Integer)

metadataRefScriptA :: ScriptHash
metadataRefScriptA = ScriptHash (bs28 0x31)

metadataRefScriptB :: ScriptHash
metadataRefScriptB = ScriptHash (bs28 0x32)

withRefInputDatumValue :: TxOutRef -> Address -> Value -> BuiltinData -> ScriptContextBuilder
withRefInputDatumValue ref addr value dat =
  withReferenceInput
    ( withOutRef ref
        <> withAddress addr
        <> withValue value
        <> withInlineDatum dat
    )

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
        ( withRewardingScript (PlutusTx.toBuiltinData seizeRedeemer) globalCred 0
            <> withWithdrawal issuerCred 0
            <> seizeInputWithMetadataBuilder metadataDatumA metadataRefScriptA
            <> seizeCorrespondingOutputWithMetadataBuilder metadataDatumB metadataRefScriptA
            <> withRefInputDatumValue paramRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)]) (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue dirNodeRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)]) (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeReferenceScriptMismatchCtx :: ScriptContext
mkGlobalSeizeReferenceScriptMismatchCtx =
  let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0
   in buildBalancedScriptContext
        ( withRewardingScript (PlutusTx.toBuiltinData seizeRedeemer) globalCred 0
            <> withWithdrawal issuerCred 0
            <> seizeInputWithMetadataBuilder metadataDatumA metadataRefScriptA
            <> seizeCorrespondingOutputWithMetadataBuilder metadataDatumA metadataRefScriptB
            <> withRefInputDatumValue paramRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)]) (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue dirNodeRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)]) (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeBurnCtx :: ScriptContext
mkGlobalSeizeBurnCtx =
  let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0
   in buildBalancedScriptContext
        ( withRewardingScript (PlutusTx.toBuiltinData seizeRedeemer) globalCred 0
            <> withWithdrawal issuerCred 0
            <> seizeInputBuilder 0
            <> withOutput (withTxOutAddress seizeInputAddr <> withTxOutValue (mkAdaValue 3_000_000))
            <> withMint (mkValue [(programmableTransferCS, TokenName "0c", -1)]) (PlutusTx.toBuiltinData ())
            <> withRefInputDatumValue paramRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)]) (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue dirNodeRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)]) (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeMintContainedCtx :: ScriptContext
mkGlobalSeizeMintContainedCtx =
  let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0
   in buildBalancedScriptContext
        ( withRewardingScript (PlutusTx.toBuiltinData seizeRedeemer) globalCred 0
            <> withWithdrawal issuerCred 0
            <> seizeInputBuilder 0
            <> withOutput (withTxOutAddress seizeInputAddr <> withTxOutValue (mkValue [(programmableTransferCS, TokenName "0c", 1)]))
            <> withOutput (withTxOutAddress seizeInputAddr <> withTxOutValue seizeInputValue)
            <> withMint (mkValue [(programmableTransferCS, TokenName "0c", 1)]) (PlutusTx.toBuiltinData ())
            <> withRefInputDatumValue paramRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)]) (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue dirNodeRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)]) (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeMintEscapeCtx :: ScriptContext
mkGlobalSeizeMintEscapeCtx =
  let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 1
   in buildBalancedScriptContext
        ( withRewardingScript (PlutusTx.toBuiltinData seizeRedeemer) globalCred 0
            <> withWithdrawal issuerCred 0
            <> seizeInputBuilder 0
            <> withOutput (withTxOutAddress seizeInputAddr <> withTxOutValue seizeInputValue)
            <> withOutput (withTxOutAddress (pubKeyAddress signerPkh) <> withTxOutValue (mkValue [(programmableTransferCS, TokenName "0c", 1)]))
            <> withMint (mkValue [(programmableTransferCS, TokenName "0c", 1)]) (PlutusTx.toBuiltinData ())
            <> withRefInputDatumValue paramRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)]) (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue dirNodeRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)]) (PlutusTx.toBuiltinData directoryProgrammableNode)
        )

mkGlobalSeizeDirectEscapeCtx :: ScriptContext
mkGlobalSeizeDirectEscapeCtx =
  let seizeRedeemer = mkSeizeActRedeemerFromRelativeInputIdxs 1 [0] 0
   in buildBalancedScriptContext
        ( withRewardingScript (PlutusTx.toBuiltinData seizeRedeemer) globalCred 0
            <> withWithdrawal issuerCred 0
            <> seizeInputBuilder 0
            <> withOutput (withTxOutAddress (pubKeyAddress signerPkh) <> withTxOutValue seizeInputValue)
            <> withRefInputDatumValue paramRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(protocolParamsCS, protocolParamsToken, 1)]) (PlutusTx.toBuiltinData protocolParamsDatum)
            <> withRefInputDatumValue dirNodeRef (pubKeyAddress signerPkh) (mkAdaValue 3_000_000 <> mkValue [(directoryNodeCS, TokenName "", 1)]) (PlutusTx.toBuiltinData directoryProgrammableNode)
        )
