{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Settings.Types.SettingsState where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift ()
import Plutarch.LedgerApi.V3 (PAddress, PCredential, PPubKeyHash)
import Plutarch.Prelude
import PlutusLedgerApi.V3 (Address, Credential, PubKeyHash)
import PlutusTx qualified
import PlutusTx.AsData (asData)
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData (makeIsDataIndexed)

-- ============================================================================
-- Plinth: MultisigScript
-- ============================================================================

data MultisigScript
  = Signature PubKeyHash
  | AllOf [MultisigScript]
  | AnyOf [MultisigScript]
  | AtLeast Integer [MultisigScript]
  | Before Integer
  | After Integer
  | ScriptWit PubKeyHash
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''MultisigScript
  [ ('Signature, 0)
  , ('AllOf, 1)
  , ('AnyOf, 2)
  , ('AtLeast, 3)
  , ('Before, 4)
  , ('After, 5)
  , ('ScriptWit, 6)
  ]

-- ============================================================================
-- Plinth: SettingsDatum
-- ============================================================================

data SettingsDatum = SettingsDatum
  { sdSettingsAdmin :: MultisigScript
  , sdMetadataAdmin :: Address
  , sdTreasuryAdmin :: MultisigScript
  , sdTreasuryAddress :: Address
  , sdTreasuryAllowance :: (Integer, Integer)
  , sdAuthorizedScoopers :: Maybe [PubKeyHash]
  , sdAuthorizedStakingKeys :: [Credential]
  , sdBaseFee :: Integer
  , sdSimpleFee :: Integer
  , sdStrategyFee :: Integer
  , sdPoolCreationFee :: Integer
  , sdExtensions :: BuiltinData
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed ''SettingsDatum [('SettingsDatum, 0)]

-- ============================================================================
-- Plinth: SettingsRedeemer
-- ============================================================================

data SettingsRedeemer
  = SettingsAdminUpdate
  | TreasuryAdminUpdate
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''SettingsRedeemer
  [ ('SettingsAdminUpdate, 0)
  , ('TreasuryAdminUpdate, 1)
  ]

-- ============================================================================
-- Plinth (AsData): MultisigScriptD
-- ============================================================================

$( asData
     [d|
       data MultisigScriptD
         = SignatureD PubKeyHash
         | AllOfD [MultisigScriptD]
         | AnyOfD [MultisigScriptD]
         | AtLeastD Integer [MultisigScriptD]
         | BeforeD Integer
         | AfterD Integer
         | ScriptWitD PubKeyHash
         deriving newtype (PlutusTx.UnsafeFromData, PlutusTx.ToData)
       |]
 )

-- ============================================================================
-- Plinth (AsData): SettingsRedeemerD
-- ============================================================================

$( asData
     [d|
       data SettingsRedeemerD
         = SettingsAdminUpdateD
         | TreasuryAdminUpdateD
         deriving newtype (PlutusTx.UnsafeFromData)
       |]
 )

-- ============================================================================
-- Plinth (AsData): SettingsDatumD
-- ============================================================================

$( asData
     [d|
       data SettingsDatumD = SettingsDatumD
         { sdSettingsAdminD :: BuiltinData
         , sdMetadataAdminD :: BuiltinData
         , sdTreasuryAdminD :: BuiltinData
         , sdTreasuryAddressD :: BuiltinData
         , sdTreasuryAllowanceD :: BuiltinData
         , sdAuthorizedScoopersD :: BuiltinData
         , sdAuthorizedStakingKeysD :: BuiltinData
         , sdBaseFeeD :: BuiltinData
         , sdSimpleFeeD :: BuiltinData
         , sdStrategyFeeD :: BuiltinData
         , sdPoolCreationFeeD :: BuiltinData
         , sdExtensionsD :: BuiltinData
         }
         deriving newtype (PlutusTx.UnsafeFromData)
       |]
 )

-- ============================================================================
-- Plutarch: PMultisigScript
-- ============================================================================

data PMultisigScript (s :: S)
  = PSignature
      { pkeyHash :: Term s (PAsData PPubKeyHash)
      }
  | PAllOf
      { pscripts :: Term s (PAsData (PBuiltinList (PAsData PMultisigScript)))
      }
  | PAnyOf
      { pscripts :: Term s (PAsData (PBuiltinList (PAsData PMultisigScript)))
      }
  | PAtLeast
      { prequired :: Term s (PAsData PInteger)
      , pscripts :: Term s (PAsData (PBuiltinList (PAsData PMultisigScript)))
      }
  | PBefore
      { ptime :: Term s (PAsData PInteger)
      }
  | PAfter
      { ptime :: Term s (PAsData PInteger)
      }
  | PScriptWit
      { pscriptHash :: Term s (PAsData PPubKeyHash)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMultisigScript)

deriving via
  DeriveDataPLiftable PMultisigScript MultisigScript
  instance
    PLiftable PMultisigScript

-- ============================================================================
-- Plutarch: PSettingsDatum
-- ============================================================================

data PSettingsDatum (s :: S) = PSettingsDatum
  { psdSettingsAdmin :: Term s (PAsData PMultisigScript)
  , psdMetadataAdmin :: Term s (PAsData PAddress)
  , psdTreasuryAdmin :: Term s (PAsData PMultisigScript)
  , psdTreasuryAddress :: Term s (PAsData PAddress)
  , psdTreasuryAllowance :: Term s (PAsData (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
  , psdAuthorizedScoopers :: Term s (PAsData PData)
  , psdAuthorizedStakingKeys :: Term s (PAsData (PBuiltinList (PAsData PCredential)))
  , psdBaseFee :: Term s (PAsData PInteger)
  , psdSimpleFee :: Term s (PAsData PInteger)
  , psdStrategyFee :: Term s (PAsData PInteger)
  , psdPoolCreationFee :: Term s (PAsData PInteger)
  , psdExtensions :: Term s (PAsData PData)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSettingsDatum)

deriving via
  DeriveDataPLiftable PSettingsDatum SettingsDatum
  instance
    PLiftable PSettingsDatum

-- ============================================================================
-- Plutarch: PSettingsRedeemer
-- ============================================================================

data PSettingsRedeemer (s :: S)
  = PSettingsAdminUpdate
  | PTreasuryAdminUpdate
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSettingsRedeemer)

deriving via
  DeriveDataPLiftable PSettingsRedeemer SettingsRedeemer
  instance
    PLiftable PSettingsRedeemer
