{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{- |
Curated DSL-jargon lists, shared between source-program jargon counting
(MeasureJargonLevel) and error-message jargon counting (MeasureErrorJargon).

Single source of truth for the curated tokens reported in the paper.
-}
module JargonLists where

data Category
  = PlutarchTermPrim
  | PlutarchOperator
  | PlutarchType
  | PlinthTermPrim
  | PlinthBuiltin
  | PlinthDataContainer
  | PlinthType
  deriving (Eq, Ord, Show)

categoryLabel :: Category -> String
categoryLabel PlutarchTermPrim = "Plutarch term primitives"
categoryLabel PlutarchOperator = "Plutarch operators"
categoryLabel PlutarchType = "Plutarch types"
categoryLabel PlinthTermPrim = "Plinth term primitives"
categoryLabel PlinthBuiltin = "Plinth BI/Builtins"
categoryLabel PlinthDataContainer = "Plinth DList/DMap/BuiltinList"
categoryLabel PlinthType = "Plinth types"

isPlutarchCategory :: Category -> Bool
isPlutarchCategory PlutarchTermPrim = True
isPlutarchCategory PlutarchOperator = True
isPlutarchCategory PlutarchType = True
isPlutarchCategory _ = False

plutarchCategories :: [Category]
plutarchCategories = [PlutarchTermPrim, PlutarchOperator, PlutarchType]

plinthCategories :: [Category]
plinthCategories =
  [PlinthTermPrim, PlinthBuiltin, PlinthDataContainer, PlinthType]

-- An Exact rule matches an UnQual identifier or operator by its base name.
-- A QualPrefix rule matches any Qual identifier whose module prefix equals the
-- given name (e.g. "BI" matches BI.head, BI.snd, BI.unsafeDataAsMap, ...).
data JargonRule
  = Exact !String !Category
  | QualPrefix !String !Category
  deriving (Show)

plutarchRules :: [JargonRule]
plutarchRules =
  map (`Exact` PlutarchTermPrim) plutarchTermPrims
    ++ map (`Exact` PlutarchOperator) plutarchOperators
    ++ map (`Exact` PlutarchType) plutarchTypes

plutarchTermPrims :: [String]
plutarchTermPrims =
  -- Core eDSL forms
  [ "plam"
  , "plet"
  , "pmatch"
  , "pif"
  , "pcond"
  , "pfix"
  , -- list ops
    "pelimList"
  , "pcons"
  , "pnil"
  , "phead"
  , "ptail"
  , -- sharing
    "phoistAcyclic"
  , -- constants
    "pconstant"
  , "pconstantInteger"
  , -- data conversion
    "pfromData"
  , "pforgetData"
  , "pdata"
  , "pto"
  , "pcon"
  , -- unsafe data casts
    "pasConstr"
  , "pasInt"
  , "pasMap"
  , "pasList"
  , "pasByteStr"
  , -- pair primitives
    "pfstBuiltin"
  , "psndBuiltin"
  , "pairDataBuiltinRaw"
  , -- errors / coercions
    "perror"
  , "punsafeCoerce"
  , "punsafeBuiltin"
  , -- numeric
    "pdiv"
  , "pmod"
  , "pquot"
  , "prem"
  , -- boolean
    "pand"
  , "por"
  , "pnot"
  , -- trace
    "ptrace"
  , "ptraceInfo"
  , "ptraceError"
  , "ptraceInfoIfFalse"
  , -- container ops
    "plookup"
  , "pcheckBinRel"
  , -- compilation entry
    "pcompile"
  ]

plutarchOperators :: [String]
plutarchOperators =
  [ "#"
  , "#$"
  , "#=="
  , "#/="
  , "#<"
  , "#<="
  , "#>"
  , "#>="
  , "#&&"
  , "#||"
  , "#<>"
  , ":-->"
  ]

-- The Plutarch and Plinth type lists are kept structurally parallel: every
-- ledger type that appears on one side has its counterpart on the other. For
-- types whose Plinth name already begins with a capital P (POSIXTime), Plutarch
-- doubles the prefix (PPOSIXTime).
plutarchTypes :: [String]
plutarchTypes =
  -- DSL-only types (no Plinth analogue; Plutarch's pure-eDSL surface)
  [ "Term"
  , "PInteger"
  , "PBool"
  , "PUnit"
  , "PData"
  , "PString"
  , "PByteString"
  , -- DSL containers (no direct Plinth analogue)
    "PAsData"
  , "PBuiltinList"
  , "PBuiltinPair"
  , "PList"
  , "PMaybe"
  , "PMaybeData"
  , "PDJust"
  , "PDNothing"
  , "PPair"
  , "PEither"
  , "PMap"
  , -- Ledger API types (parallel to Plinth list below)
    "PScriptContext"
  , "PTxInfo"
  , "PTxInInfo"
  , "PTxOut"
  , "PValue"
  , "PLovelace"
  , "PAddress"
  , "PPubKeyHash"
  , "PScriptHash"
  , "PCurrencySymbol"
  , "PTokenName"
  , "PStakingCredential"
  , "PStakingHash"
  , "PStakingPtr"
  , "PCredential"
  , "PPubKeyCredential"
  , "PScriptCredential"
  , "PInterval"
  , "PLowerBound"
  , "PUpperBound"
  , "PFinite"
  , "PPosInf"
  , "PNegInf"
  , "POutputDatum"
  , "PNoOutputDatum"
  , "POutputDatumHash"
  , "PRedeemer"
  , "PDatum"
  , "PDatumHash"
  , "PScriptInfo"
  , "PScriptPurpose"
  , "PSpendingScript"
  , "PMintingScript"
  , "PRewardingScript"
  , "PCertifyingScript"
  , "PVotingScript"
  , "PProposingScript"
  , "PTxOutRef"
  , "PTxId"
  , "PPOSIXTime"
  , "PPOSIXTimeRange"
  , "PTxCert"
  , "PTxCertRegStaking"
  , "PTxCertUnRegStaking"
  , "PTxCertDelegStaking"
  , "PTxCertRegDeleg"
  ]

plinthRules :: [JargonRule]
plinthRules =
  map (`Exact` PlinthTermPrim) plinthTermPrims
    ++ map (`QualPrefix` PlinthBuiltin) plinthBuiltinPrefixes
    ++ map (`QualPrefix` PlinthDataContainer) plinthDataContainerPrefixes
    ++ map (`Exact` PlinthType) plinthTypes

plinthTermPrims :: [String]
plinthTermPrims =
  [ "plinthc"
  , "unsafeFromBuiltinData"
  , "toBuiltinData"
  , "fromBuiltinData"
  , "getRedeemer"
  , "getLovelace"
  , "matchInterval"
  , "matchLowerBound"
  , "matchExtended"
  , "matchOutputDatum"
  ]

plinthBuiltinPrefixes :: [String]
plinthBuiltinPrefixes =
  -- These are the qualified import aliases used in the Plinth files.
  -- Every reference of the form Prefix.something counts.
  [ "BI"
  , "Builtins"
  ]

plinthDataContainerPrefixes :: [String]
plinthDataContainerPrefixes =
  [ "DList"
  , "DMap"
  , "BuiltinList"
  ]

-- The Plinth type list is structurally parallel to plutarchTypes above. The
-- "Plinth-side glue" entries cover types that exist on Plinth's side but have
-- no Plutarch analogue (BuiltinData, CompiledCode); the rest are the unprefixed
-- parallels of the P-prefixed Plutarch ledger types so the comparison stays
-- symmetric. Plinth's PlutusTx.Data.List and PlutusTx.Data.AssocMap re-export
-- a Data-backed List/Map; their unqualified parallels of PList/PMap appear too.
plinthTypes :: [String]
plinthTypes =
  -- Plinth-side glue (no Plutarch analogue)
  [ "BuiltinData"
  , "CompiledCode"
  , -- Ledger API types (parallel to Plutarch list above)
    "ScriptContext"
  , "TxInfo"
  , "TxInInfo"
  , "TxOut"
  , "Value"
  , "Lovelace"
  , "Address"
  , "PubKeyHash"
  , "ScriptHash"
  , "CurrencySymbol"
  , "TokenName"
  , "StakingCredential"
  , "StakingHash"
  , "StakingPtr"
  , "Credential"
  , "PubKeyCredential"
  , "ScriptCredential"
  , "Interval"
  , "LowerBound"
  , "UpperBound"
  , "Finite"
  , "PosInf"
  , "NegInf"
  , "OutputDatum"
  , "NoOutputDatum"
  , "OutputDatumHash"
  , "Redeemer"
  , "Datum"
  , "DatumHash"
  , "ScriptInfo"
  , "ScriptPurpose"
  , "SpendingScript"
  , "MintingScript"
  , "RewardingScript"
  , "CertifyingScript"
  , "VotingScript"
  , "ProposingScript"
  , "TxOutRef"
  , "TxId"
  , "POSIXTime"
  , "POSIXTimeRange"
  , "TxCert"
  , "TxCertRegStaking"
  , "TxCertUnRegStaking"
  , "TxCertDelegStaking"
  , "TxCertRegDeleg"
  ]
