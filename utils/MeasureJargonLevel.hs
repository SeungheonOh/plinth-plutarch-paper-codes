{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.Data (Data)
import Data.Generics (everything, mkQ)
import Data.List (foldl', sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import JargonLists
import Language.Haskell.Exts as HSE
import System.FilePath (takeFileName)
import Text.Printf (printf)

-- ============================================================================
-- 1. AST traversal
-- ============================================================================

qnameStrings :: QName l -> (Maybe String, String)
qnameStrings (Qual _ (ModuleName _ m) n) = (Just m, nameString n)
qnameStrings (UnQual _ n) = (Nothing, nameString n)
qnameStrings (Special _ sc) = (Nothing, specialConString sc)

nameString :: Name l -> String
nameString (Ident _ s) = s
nameString (Symbol _ s) = s

specialConString :: SpecialCon l -> String
specialConString UnitCon{} = "()"
specialConString ListCon{} = "[]"
specialConString FunCon{} = "->"
specialConString (TupleCon _ _ n) = "(" ++ replicate (n - 1) ',' ++ ")"
specialConString Cons{} = ":"
specialConString UnboxedSingleCon{} = "(# #)"
specialConString ExprHole{} = "_"

matchQName :: [JargonRule] -> QName l -> Maybe Category
matchQName rules q =
  let (mqual, base) = qnameStrings q
   in case mqual of
        Just qual ->
          let qualMatches = [c | QualPrefix p c <- rules, p == qual]
           in case qualMatches of
                (c : _) -> Just c
                [] -> exactMatch rules base
        Nothing -> exactMatch rules base
 where
  exactMatch rs b =
    case [c | Exact n c <- rs, n == b] of
      (c : _) -> Just c
      [] -> Nothing

allQNames :: forall a. (Data a) => a -> [QName SrcSpanInfo]
allQNames = everything (++) (mkQ [] go)
 where
  go :: QName SrcSpanInfo -> [QName SrcSpanInfo]
  go q = [q]

-- ============================================================================
-- 2. Counts
-- ============================================================================

data JargonCounts = JargonCounts
  { jcByCategory :: !(Map Category Int)
  , jcTotal :: !Int
  }
  deriving (Show)

emptyCounts :: JargonCounts
emptyCounts = JargonCounts Map.empty 0

bumpCounts :: Category -> JargonCounts -> JargonCounts
bumpCounts c (JargonCounts m t) =
  JargonCounts (Map.insertWith (+) c 1 m) (t + 1)

countCategory :: Category -> JargonCounts -> Int
countCategory c (JargonCounts m _) = Map.findWithDefault 0 c m

countMany :: [Category] -> JargonCounts -> Int
countMany cs jc = sum [countCategory c jc | c <- cs]

-- ============================================================================
-- 3. eLOC (effective lines of code), mirroring MeasureCodeSize.hs
-- ============================================================================

declLines :: SrcSpanInfo -> Int
declLines (SrcSpanInfo (SrcSpan _ sl _ el _) _) = el - sl + 1

pragmaLns :: ModulePragma SrcSpanInfo -> Int
pragmaLns (LanguagePragma l _) = declLines l
pragmaLns (OptionsPragma l _ _) = declLines l
pragmaLns (AnnModulePragma l _) = declLines l

commentLns :: Comment -> Int
commentLns (Comment _ (SrcSpan _ sl _ el _) _) = el - sl + 1

-- ============================================================================
-- 4. Parsing
-- ============================================================================

parseMode :: FilePath -> ParseMode
parseMode path =
  defaultParseMode
    { parseFilename = path
    , extensions =
        glasgowExts
          ++ [ EnableExtension HSE.TemplateHaskell
             , EnableExtension QuasiQuotes
             , EnableExtension MultiParamTypeClasses
             , EnableExtension ExplicitForAll
             , EnableExtension PatternSynonyms
             , EnableExtension DataKinds
             , EnableExtension TypeFamilies
             , EnableExtension KindSignatures
             , EnableExtension RankNTypes
             , EnableExtension FlexibleContexts
             , EnableExtension FlexibleInstances
             , EnableExtension GADTs
             , EnableExtension ScopedTypeVariables
             , EnableExtension StandaloneDeriving
             , EnableExtension DerivingVia
             , EnableExtension DerivingStrategies
             , EnableExtension OverloadedStrings
             , EnableExtension TypeOperators
             , EnableExtension LambdaCase
             , EnableExtension ViewPatterns
             , EnableExtension OverloadedLabels
             , EnableExtension NamedFieldPuns
             , EnableExtension RecordWildCards
             , EnableExtension BangPatterns
             , EnableExtension ImpredicativeTypes
             , EnableExtension PartialTypeSignatures
             , EnableExtension UndecidableInstances
             , EnableExtension GeneralizedNewtypeDeriving
             , EnableExtension DeriveGeneric
             , EnableExtension DeriveAnyClass
             , EnableExtension TypeApplications
             ]
    , fixities = Nothing
    }

-- HSE's older parsers expected `import qualified Foo`. The repository uses
-- ImportQualifiedPost (`import Foo qualified`), so we swap the words back to
-- the prefix form before handing it to HSE. We do not strip TypeApplications,
-- since HSE supports them and stripping would undercount Plutarch types
-- appearing inside `@(...)` annotations.
preprocessSource :: String -> String
preprocessSource = unlines . map fixLine . lines
 where
  fixLine line
    | take 7 line == "import " =
        let ws = words line
         in case ws of
              ("import" : modName : "qualified" : rest) ->
                unwords ("import" : "qualified" : modName : rest)
              _ -> line
    | otherwise = line

-- ============================================================================
-- 5. File analysis
-- ============================================================================

data Side = PlutarchSide | PlinthSide
  deriving (Eq, Show)

data FileResult = FileResult
  { frFile :: !FilePath
  , frTotalLines :: !Int
  , frBlankLines :: !Int
  , frPragmaLines :: !Int
  , frModuleLines :: !Int
  , frImportLines :: !Int
  , frCommentLines :: !Int
  , frEloc :: !Int
  , frTokens :: !Int
  , frJargon :: !JargonCounts
  }

analyze :: Side -> FilePath -> IO FileResult
analyze side path = do
  raw <- readFile path
  let src = preprocessSource raw
      rawLines = lines raw
      totalLines = length rawLines
      blankLines = length (filter (all (== ' ')) rawLines)
      -- Source-code "tokens" per the paper methodology: whitespace-separated
      -- words across the entire source file. `words` splits on any whitespace
      -- (spaces and newlines included).
      tokens = length (words raw)
      mode = parseMode path
  case parseFileContentsWithComments mode src of
    ParseFailed loc msg ->
      error $ "Parse failed for " ++ path ++ " at " ++ show loc ++ ": " ++ msg
    ParseOk (Module _ mhead pragmas imports decls, comments) -> do
      let pragmaLines = sum (map pragmaLns pragmas)
          headLines = maybe 0 (\(HSE.ModuleHead l _ _ _) -> declLines l) mhead
          importLines = sum [declLines l | ImportDecl l _ _ _ _ _ _ _ <- imports]
          commentLineCount = sum (map commentLns comments)
          eloc = totalLines - blankLines - pragmaLines - headLines - importLines - commentLineCount
          rules = case side of
            PlutarchSide -> plutarchRules
            PlinthSide -> plinthRules
          -- Only walk decls so pragmas, module head, imports, and comments are
          -- excluded from jargon counting by construction.
          qnames = concatMap allQNames decls
          hits = mapMaybe (matchQName rules) qnames
          jc = foldl' (flip bumpCounts) emptyCounts hits
      pure
        FileResult
          { frFile = takeFileName path
          , frTotalLines = totalLines
          , frBlankLines = blankLines
          , frPragmaLines = pragmaLines
          , frModuleLines = headLines
          , frImportLines = importLines
          , frCommentLines = commentLineCount
          , frEloc = eloc
          , frTokens = tokens
          , frJargon = jc
          }
    ParseOk _ ->
      error $ "Unexpected parse result for " ++ path

-- ============================================================================
-- 6. Validator pairs (Contracts/*.hs only)
-- ============================================================================

data Pair = Pair
  { pairName :: String
  , pairPlutarchPath :: FilePath
  , pairPlinthPath :: FilePath
  }

allPairs :: [Pair]
allPairs =
  [ Pair
      "Smart Tokens"
      "src/SmartTokens/Contracts/ProgrammableLogicBase.hs"
      "src/SmartTokens/Contracts/ProgrammableLogicBasePlinth.hs"
  , Pair
      "Guardrail"
      "src/Constitution/Contracts/ConstitutionSorted.hs"
      "src/Constitution/Contracts/ConstitutionSortedPlinth.hs"
  , Pair
      "Crowdfund"
      "src/Crowdfund/Contracts/Crowdfund.hs"
      "src/Crowdfund/Contracts/CrowdfundPlinth.hs"
  , Pair
      "Vesting"
      "src/Vesting/Contracts/Vesting.hs"
      "src/Vesting/Contracts/VestingPlinth.hs"
  , Pair
      "SundaeSwap NFT"
      "src/Settings/Contracts/Settings.hs"
      "src/Settings/Contracts/SettingsPlinth.hs"
  , Pair
      "Certifying"
      "src/Certifying/Contracts/Certifying.hs"
      "src/Certifying/Contracts/CertifyingPlinth.hs"
  , Pair
      "Voting"
      "src/Voting/Contracts/Voting.hs"
      "src/Voting/Contracts/VotingPlinth.hs"
  , Pair
      "Hydra Head"
      "src/Hydra/Contracts/Head.hs"
      "src/Hydra/Contracts/HeadPlinth.hs"
  ]

-- ============================================================================
-- 7. Reporting
-- ============================================================================

ratioStr :: Int -> Int -> String
ratioStr num denom
  | denom > 0 = printf "%.2fx" (fromIntegral num / fromIntegral denom :: Double)
  | num > 0 = "N/A"
  | otherwise = "-"

densityStr :: Int -> Int -> String
densityStr jargon eloc
  | eloc > 0 = printf "%.2f" (fromIntegral jargon / fromIntegral eloc :: Double)
  | otherwise = "-"

printPair :: Pair -> IO (String, FileResult, FileResult)
printPair Pair{..} = do
  fp <- analyze PlutarchSide pairPlutarchPath
  ft <- analyze PlinthSide pairPlinthPath
  putStrLn ""
  putStrLn $ "=== " ++ pairName ++ " ==="
  putStrLn ""
  printf
    "  %-26s  %10s  %10s  %10s\n"
    ("Metric" :: String)
    ("Plutarch" :: String)
    ("Plinth" :: String)
    ("ratio" :: String)
  putStrLn (replicate 64 '-')

  let pTotal = jcTotal (frJargon fp)
      tTotal = jcTotal (frJargon ft)
  printf
    "  %-26s  %10d  %10d  %10s\n"
    ("Jargon tokens (total)" :: String)
    pTotal
    tTotal
    (ratioStr tTotal pTotal)
  printf
    "  %-26s  %10d  %10d  %10s\n"
    ("eLOC (denominator)" :: String)
    (frEloc fp)
    (frEloc ft)
    (ratioStr (frEloc ft) (frEloc fp))
  printf
    "  %-26s  %10d  %10d  %10s\n"
    ("Source tokens (denom)" :: String)
    (frTokens fp)
    (frTokens ft)
    (ratioStr (frTokens ft) (frTokens fp))
  printf
    "  %-26s  %10s  %10s  %10s\n"
    ("Jargon density (per eLOC)" :: String)
    (densityStr pTotal (frEloc fp))
    (densityStr tTotal (frEloc ft))
    ("" :: String)
  printf
    "  %-26s  %10s  %10s  %10s\n"
    ("Jargon density (per token)" :: String)
    (densityStr pTotal (frTokens fp))
    (densityStr tTotal (frTokens ft))
    ("" :: String)

  putStrLn ""
  putStrLn "  Plutarch breakdown:"
  mapM_
    ( \c ->
        printf
          "    %-26s  %10d\n"
          (categoryLabel c)
          (countCategory c (frJargon fp))
    )
    plutarchCategories

  putStrLn ""
  putStrLn "  Plinth breakdown:"
  mapM_
    ( \c ->
        printf
          "    %-26s  %10d\n"
          (categoryLabel c)
          (countCategory c (frJargon ft))
    )
    plinthCategories

  pure (pairName, fp, ft)

printSummary :: [(String, FileResult, FileResult)] -> IO ()
printSummary rows = do
  putStrLn ""
  putStrLn "Summary: source program jargon level"
  putStrLn (replicate 110 '=')
  printf
    "  %-18s | %10s | %10s | %8s | %10s | %10s | %10s | %10s\n"
    ("Validator" :: String)
    ("Plutarch" :: String)
    ("Plinth" :: String)
    ("ratio" :: String)
    ("plut/eLOC" :: String)
    ("plin/eLOC" :: String)
    ("plut/tok" :: String)
    ("plin/tok" :: String)
  putStrLn (replicate 110 '-')
  let summarizeRow (n, fp, ft) = do
        let p = jcTotal (frJargon fp)
            t = jcTotal (frJargon ft)
        printf
          "  %-18s | %10d | %10d | %8s | %10s | %10s | %10s | %10s\n"
          n
          p
          t
          (ratioStr t p)
          (densityStr p (frEloc fp))
          (densityStr t (frEloc ft))
          (densityStr p (frTokens fp))
          (densityStr t (frTokens ft))
  mapM_ summarizeRow rows
  putStrLn (replicate 110 '-')
  let totalP = sum [jcTotal (frJargon fp) | (_, fp, _) <- rows]
      totalT = sum [jcTotal (frJargon ft) | (_, _, ft) <- rows]
      totalPEloc = sum [frEloc fp | (_, fp, _) <- rows]
      totalTEloc = sum [frEloc ft | (_, _, ft) <- rows]
      totalPTokens = sum [frTokens fp | (_, fp, _) <- rows]
      totalTTokens = sum [frTokens ft | (_, _, ft) <- rows]
  printf
    "  %-18s | %10d | %10d | %8s | %10s | %10s | %10s | %10s\n"
    ("TOTAL" :: String)
    totalP
    totalT
    (ratioStr totalT totalP)
    (densityStr totalP totalPEloc)
    (densityStr totalT totalTEloc)
    (densityStr totalP totalPTokens)
    (densityStr totalT totalTTokens)
  putStrLn (replicate 110 '=')

  putStrLn ""
  putStrLn "Per-category aggregate"
  putStrLn (replicate 56 '=')
  printf
    "  %-30s | %10s\n"
    ("Category" :: String)
    ("Total" :: String)
  putStrLn (replicate 56 '-')
  let allCats = sort (plutarchCategories ++ plinthCategories)
  mapM_
    ( \c -> do
        let total =
              sum
                [ countCategory c (frJargon (if isPlutarchCategory c then fp else ft))
                | (_, fp, ft) <- rows
                ]
        printf "  %-30s | %10d\n" (categoryLabel c) total
    )
    allCats
  putStrLn (replicate 56 '=')
  putStrLn ""

main :: IO ()
main = do
  rows <- mapM printPair allPairs
  printSummary rows
