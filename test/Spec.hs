{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Main (main) where

import Plutarch.Internal.Term (Config (NoTracing), compile)
import Plutarch.Script (Script)
import ProgrammableTokens.Test.ProgrammableLogicGlobal qualified as ProgrammableLogicGlobal
import SmartTokens.Contracts.ProgrammableLogicBase (mkProgrammableLogicGlobal)
import SmartTokens.Contracts.ProgrammableLogicBasePlinth (plinthProgrammableLogicGlobalScript)
import Test.Tasty (defaultMain, testGroup)

plutarchGlobalScript :: Script
plutarchGlobalScript =
  either (error . ("compile failed: " <>) . show) id (compile NoTracing mkProgrammableLogicGlobal)

main :: IO ()
main =
  defaultMain $
    testGroup
      "CIP-143 Conformance Tests"
      [ ProgrammableLogicGlobal.mkGlobalTests "Plutarch" plutarchGlobalScript
      , ProgrammableLogicGlobal.mkGlobalTests "Plinth (PlutusTx)" plinthProgrammableLogicGlobalScript
      ]
