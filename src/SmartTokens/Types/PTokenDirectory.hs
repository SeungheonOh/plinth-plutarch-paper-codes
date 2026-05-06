{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module SmartTokens.Types.PTokenDirectory (
  DirectorySetNode (..),
  DirectorySetNodeD,
  keyD,
  nextD,
  transferLogicScriptD,
  issuerLogicScriptD,
  globalStateCSD,
  PDirectorySetNode (..),
  PBlacklistNode (..),
  BlacklistNode (..),
  isHeadNode,
  isTailNode,
  pemptyBSData,
  pemptyCSData,
  pmkDirectorySetNode,
  pisInsertedOnNode,
  pisInsertedNode,
  pisEmptyNode,
  pmkBlacklistNode,
  pisInsertedOnBlacklistNode,
  pisInsertedBlacklistNode,
  pisEmptyBlacklistNode,
  pisBlacklistTailNode,
  pemptyBlacklistNode,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Evaluate (unsafeEvalTerm)
import Plutarch.Internal.Lift ()
import Plutarch.Internal.Term (Config (NoTracing))
import Plutarch.LedgerApi.V3 (PCredential, PCurrencySymbol)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.Data.V3 qualified as DV3
import PlutusLedgerApi.V3 (BuiltinByteString, Credential, CurrencySymbol)
import PlutusTx (Data (B, Constr))
import PlutusTx qualified
import PlutusTx.AsData (asData)

-- ============================================================================
-- Plutarch helpers (defined before TH splices to avoid staging issues)
-- ============================================================================

pmkBuiltinList :: [Term s PData] -> Term s (PBuiltinList PData)
pmkBuiltinList = foldr (\x acc -> pcons # x # acc) pnil

pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList
      (pelimList (\_ _ -> ptraceInfoError "List contains more than one element."))
      (ptraceInfoError "List is empty.")
      xs

pemptyBSData :: Term s (PAsData PByteString)
pemptyBSData = unsafeEvalTerm NoTracing (punsafeCoerce (pconstant @PData $ B ""))

pemptyCSData :: Term s (PAsData PCurrencySymbol)
pemptyCSData = unsafeEvalTerm NoTracing (punsafeCoerce (pconstant @PData $ B ""))

ptailNextData :: Term s (PAsData PCurrencySymbol)
ptailNextData = unsafeEvalTerm NoTracing (punsafeCoerce $ pdata (phexByteStr "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))

ptailBlackListNext :: Term s (PAsData PByteString)
ptailBlackListNext = unsafeEvalTerm NoTracing (punsafeCoerce $ pdata (phexByteStr "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))

-- ============================================================================
-- Plinth: BlacklistNode
-- ============================================================================

data BlacklistNode
  = BlacklistNode
  { blnKey :: BuiltinByteString
  , blnNext :: BuiltinByteString
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''BlacklistNode [('BlacklistNode, 0)]

-- ============================================================================
-- Plutarch: PBlacklistNode
-- ============================================================================

data PBlacklistNode (s :: S)
  = PBlacklistNode
  { pblnKey :: Term s (PAsData PByteString)
  , pblnNext :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow, PEq)
  deriving (PlutusType) via (DeriveAsDataStruct PBlacklistNode)

deriving via
  DeriveDataPLiftable PBlacklistNode BlacklistNode
  instance
    PLiftable PBlacklistNode

pmkBlacklistNode :: Term s (PAsData PByteString :--> PAsData PByteString :--> PAsData PBlacklistNode)
pmkBlacklistNode = phoistAcyclic $
  plam $ \key_ next_ ->
    punsafeCoerce $ plistData # pmkBuiltinList [pforgetData key_, pforgetData next_]

pisInsertedOnBlacklistNode :: Term s (PAsData PByteString :--> PAsData PByteString :--> PAsData PBlacklistNode :--> PBool)
pisInsertedOnBlacklistNode = phoistAcyclic $
  plam $ \insertedKey coveringKey outputNode ->
    let expectedDirectoryNode = pmkBlacklistNode # coveringKey # insertedKey
     in outputNode #== expectedDirectoryNode

pisInsertedBlacklistNode :: Term s (PAsData PByteString :--> PAsData PByteString :--> PAsData PBlacklistNode :--> PBool)
pisInsertedBlacklistNode = phoistAcyclic $
  plam $ \insertedKey coveringNext outputNode ->
    let expectedDirectoryNode = pmkBlacklistNode # insertedKey # coveringNext
     in outputNode #== expectedDirectoryNode

pemptyBlacklistNode :: Term s (PAsData PBlacklistNode)
pemptyBlacklistNode = punsafeCoerce $ plistData # pmkBuiltinList [pforgetData pemptyBSData, pforgetData ptailBlackListNext]

pisBlacklistTailNode :: Term s (PAsData PBlacklistNode) -> Term s PBool
pisBlacklistTailNode node =
  pmatch (pfromData node) $ \(PBlacklistNode{pblnNext}) ->
    pblnNext #== ptailBlackListNext

pisEmptyBlacklistNode :: Term s (PAsData PBlacklistNode) -> Term s PBool
pisEmptyBlacklistNode node =
  node #== pemptyBlacklistNode

-- ============================================================================
-- Plinth: DirectorySetNode
-- ============================================================================

data DirectorySetNode = DirectorySetNode
  { key :: CurrencySymbol
  , next :: CurrencySymbol
  , transferLogicScript :: Credential
  , issuerLogicScript :: Credential
  , globalStateCS :: CurrencySymbol
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''DirectorySetNode [('DirectorySetNode, 0)]

-- ============================================================================
-- Plinth (AsData): DirectorySetNodeD
-- ============================================================================

$( asData
     [d|
       data DirectorySetNodeD = DirectorySetNodeD
         { keyD :: DV3.CurrencySymbol
         , nextD :: DV3.CurrencySymbol
         , transferLogicScriptD :: DV3.Credential
         , issuerLogicScriptD :: DV3.Credential
         , globalStateCSD :: DV3.CurrencySymbol
         }
         deriving newtype (PlutusTx.UnsafeFromData, PlutusTx.ToData)
       |]
 )

-- ============================================================================
-- Plutarch: PDirectorySetNode
-- ============================================================================

data PDirectorySetNode (s :: S)
  = PDirectorySetNode
  { pkey :: Term s (PAsData PCurrencySymbol)
  , pnext :: Term s (PAsData PCurrencySymbol)
  , ptransferLogicScript :: Term s (PAsData PCredential)
  , pissuerLogicScript :: Term s (PAsData PCredential)
  , pglobalStateCS :: Term s (PAsData PCurrencySymbol)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDirectorySetNode)

deriving via
  DeriveDataPLiftable PDirectorySetNode DirectorySetNode
  instance
    PLiftable PDirectorySetNode

isHeadNode :: Term s (PAsData PDirectorySetNode) -> Term s PBool
isHeadNode node =
  pmatch (pfromData node) $ \node' ->
    pkey node' #== pemptyCSData

isTailNode :: Term s (PAsData PDirectorySetNode) -> Term s PBool
isTailNode node =
  pmatch (pfromData node) $ \node' ->
    pnext node' #== ptailNextData

emptyNode :: Term s (PAsData PDirectorySetNode)
emptyNode =
  let nullTransferLogicCred = pconstant (Constr 0 [B ""])
      nullIssuerLogicCred = pconstant (Constr 0 [B ""])
   in punsafeCoerce $ plistData # pmkBuiltinList [pforgetData pemptyBSData, pforgetData ptailNextData, nullTransferLogicCred, nullIssuerLogicCred, pforgetData pemptyBSData]

pisEmptyNode :: Term s (PAsData PDirectorySetNode) -> Term s PBool
pisEmptyNode node =
  node #== emptyNode

pmkDirectorySetNode :: Term s (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PCurrencySymbol :--> PAsData PDirectorySetNode)
pmkDirectorySetNode = phoistAcyclic $
  plam $ \key_ next_ transferLogicCred issuerLogicCred globalStateCS_ ->
    punsafeCoerce $ plistData # pmkBuiltinList [pforgetData key_, pforgetData next_, pforgetData transferLogicCred, pforgetData issuerLogicCred, pforgetData globalStateCS_]

pisInsertedOnNode :: Term s (PAsData PByteString :--> PAsData PByteString :--> PAsData PCredential :--> PAsData PCredential :--> PAsData PCurrencySymbol :--> PAsData PDirectorySetNode :--> PBool)
pisInsertedOnNode = phoistAcyclic $
  plam $ \insertedKey coveringKey transferLogicCred issuerLogicCred globalCS outputNode ->
    let expectedDirectoryNode = pmkDirectorySetNode # coveringKey # insertedKey # transferLogicCred # issuerLogicCred # globalCS
     in outputNode #== expectedDirectoryNode

pisInsertedNode :: Term s (PAsData PByteString :--> PAsData PByteString :--> PAsData PDirectorySetNode :--> PBool)
pisInsertedNode = phoistAcyclic $
  plam $ \insertedKey coveringNext outputNode ->
    pmatch (pfromData outputNode) $ \(PDirectorySetNode{ptransferLogicScript, pissuerLogicScript, pglobalStateCS}) ->
      let transferLogicCred_ = ptraceInfoShowId ptransferLogicScript
          issuerLogicCred_ = ptraceInfoShowId pissuerLogicScript
          expectedDirectoryNode =
            pmkDirectorySetNode # insertedKey # coveringNext # pdeserializeDirectoryCredential transferLogicCred_ # pdeserializeDirectoryCredential issuerLogicCred_ # pdeserializeCurrencySymbol pglobalStateCS
       in outputNode #== expectedDirectoryNode

pdeserializeDirectoryCredential :: Term s (PAsData PCredential) -> Term s (PAsData PCredential)
pdeserializeDirectoryCredential term =
  plet (pasConstr # pforgetData term) $ \constrPair ->
    plet (pfstBuiltin # constrPair) $ \constrIdx ->
      pif
        (plengthBS # (pasByteStr # (pheadSingleton # (psndBuiltin # constrPair))) #<= 28)
        ( pcond
            [ (constrIdx #== 0, term)
            , (constrIdx #== 1, term)
            ]
            (ptraceInfoError "Invalid credential")
        )
        (ptraceInfoError $ pconstant "Invalid credential len" <> pshow (plengthBS # (pasByteStr # (pheadSingleton # (psndBuiltin # constrPair)))))

pdeserializeCurrencySymbol :: Term s (PAsData PCurrencySymbol) -> Term s (PAsData PCurrencySymbol)
pdeserializeCurrencySymbol term =
  plet (pasByteStr # pforgetData term) $ \bstr ->
    pif
      (plengthBS # bstr #<= 28)
      term
      (ptraceInfoError $ pconstant "Invalid CurrencySymbol len" <> pshow (plengthBS # bstr))
