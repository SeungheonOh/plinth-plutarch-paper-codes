{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Main (main) where

import Plutarch.Internal.Term (Config (NoTracing), compile)
import Plutarch.Script (Script)

import Constitution.Contracts.ConstitutionSorted (mkConstitutionValidator)
import Constitution.Contracts.ConstitutionSortedPlinth (plinthConstitutionScript)
import Constitution.Test.ConstitutionSorted qualified as ConstitutionSorted
import Crowdfund.Contracts.Crowdfund (mkCrowdfundValidator)
import Crowdfund.Contracts.CrowdfundPlinth (plinthCrowdfundScript)
import Crowdfund.Test.Crowdfund qualified as Crowdfund
import Hydra.Contracts.Head (mkHeadValidator)
import Hydra.Contracts.HeadPlinth (plinthHeadScript)
import Hydra.Test.Head qualified as HydraHead
import ProgrammableTokens.Test.ProgrammableLogicGlobal qualified as ProgrammableLogicGlobal
import Settings.Contracts.Settings (mkSettingsValidator)
import Settings.Contracts.SettingsPlinth (plinthSettingsScript)
import Settings.Test.Settings qualified as Settings
import SmartTokens.Contracts.ProgrammableLogicBase (mkProgrammableLogicGlobal)
import SmartTokens.Contracts.ProgrammableLogicBasePlinth (plinthProgrammableLogicGlobalScript)
import Test.Tasty (defaultMain, testGroup)
import Vesting.Contracts.Vesting (mkVestingValidator)
import Vesting.Contracts.VestingPlinth (plinthVestingScript)
import Vesting.Test.Vesting qualified as Vesting

plutarchGlobalScript :: Script
plutarchGlobalScript =
  either (error . ("compile failed: " <>) . show) id (compile NoTracing mkProgrammableLogicGlobal)

plutarchHeadScript :: Script
plutarchHeadScript =
  either (error . ("compile failed: " <>) . show) id (compile NoTracing mkHeadValidator)

plutarchConstitutionScript :: Script
plutarchConstitutionScript =
  either (error . ("compile failed: " <>) . show) id (compile NoTracing mkConstitutionValidator)

plutarchCrowdfundScript :: Script
plutarchCrowdfundScript =
  either (error . ("compile failed: " <>) . show) id (compile NoTracing mkCrowdfundValidator)

plutarchSettingsScript :: Script
plutarchSettingsScript =
  either (error . ("compile failed: " <>) . show) id (compile NoTracing mkSettingsValidator)

plutarchVestingScript :: Script
plutarchVestingScript =
  either (error . ("compile failed: " <>) . show) id (compile NoTracing mkVestingValidator)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Conformance Tests"
      [ testGroup
          "CIP-143 ProgrammableLogicGlobal"
          [ ProgrammableLogicGlobal.mkGlobalTests "Plutarch" plutarchGlobalScript
          , ProgrammableLogicGlobal.mkGlobalTests "Plinth (PlutusTx)" plinthProgrammableLogicGlobalScript
          ]
      , testGroup
          "Hydra Head Validator"
          [ HydraHead.mkHeadTests "Plutarch" plutarchHeadScript
          , HydraHead.mkHeadTests "Plinth (PlutusTx)" plinthHeadScript
          , HydraHead.mkHeadConformanceTests plutarchHeadScript plinthHeadScript
          ]
      , testGroup
          "Constitution Sorted Validator"
          [ ConstitutionSorted.mkConstitutionTests "Plutarch" plutarchConstitutionScript
          , ConstitutionSorted.mkConstitutionTests "Plinth (PlutusTx)" plinthConstitutionScript
          , ConstitutionSorted.mkConstitutionConformanceTests plutarchConstitutionScript plinthConstitutionScript
          ]
      , testGroup
          "Crowdfund Validator"
          [ Crowdfund.mkCrowdfundTests "Plutarch" plutarchCrowdfundScript
          , Crowdfund.mkCrowdfundTests "Plinth (PlutusTx)" plinthCrowdfundScript
          , Crowdfund.mkCrowdfundConformanceTests plutarchCrowdfundScript plinthCrowdfundScript
          ]
      , testGroup
          "Settings Validator"
          [ Settings.mkSettingsTests "Plutarch" plutarchSettingsScript
          , Settings.mkSettingsTests "Plinth (PlutusTx)" plinthSettingsScript
          , Settings.mkSettingsConformanceTests plutarchSettingsScript plinthSettingsScript
          ]
      , testGroup
          "Vesting Validator"
          [ Vesting.mkVestingTests "Plutarch" plutarchVestingScript
          , Vesting.mkVestingTests "Plinth (PlutusTx)" plinthVestingScript
          , Vesting.mkVestingConformanceTests plutarchVestingScript plinthVestingScript
          ]
      ]
