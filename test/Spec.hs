{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Main (main) where

import Plutarch.Internal.Term (Config (NoTracing), compile)
import Plutarch.Script (Script)

import Hydra.Contracts.Head (mkHeadValidator)
import Hydra.Contracts.HeadPlinth (plinthHeadScript)
import Hydra.Test.Head qualified as HydraHead
import ProgrammableTokens.Test.ProgrammableLogicGlobal qualified as ProgrammableLogicGlobal
import SmartTokens.Contracts.ProgrammableLogicBase (mkProgrammableLogicGlobal)
import SmartTokens.Contracts.ProgrammableLogicBasePlinth (plinthProgrammableLogicGlobalScript)
import Test.Tasty (defaultMain, testGroup)

plutarchGlobalScript :: Script
plutarchGlobalScript =
  either (error . ("compile failed: " <>) . show) id (compile NoTracing mkProgrammableLogicGlobal)

plutarchHeadScript :: Script
plutarchHeadScript =
  either (error . ("compile failed: " <>) . show) id (compile NoTracing mkHeadValidator)

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
          ]
      ]
