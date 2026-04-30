{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.List (foldl', intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Language.Haskell.Exts as HSE
import System.FilePath (takeFileName)
import Text.Printf (printf)

data Section
  = SPragmas
  | SModuleHead
  | SImports
  | STypeSignatures
  | SFunctionBindings
  | SDataDeclarations
  | SClassInstances
  | SDerivingDeclarations
  | STemplateHaskell
  | SComments
  | SOther
  deriving (Eq, Ord, Show)

sectionLabel :: Section -> String
sectionLabel SPragmas = "Pragmas"
sectionLabel SModuleHead = "Module head"
sectionLabel SImports = "Imports"
sectionLabel STypeSignatures = "Type signatures"
sectionLabel SFunctionBindings = "Function bindings"
sectionLabel SDataDeclarations = "Data declarations"
sectionLabel SClassInstances = "Class/instance decls"
sectionLabel SDerivingDeclarations = "Deriving declarations"
sectionLabel STemplateHaskell = "Template Haskell"
sectionLabel SComments = "Comments"
sectionLabel SOther = "Other"

data Metrics = Metrics
  { mLines :: !Int
  , mCount :: !Int
  }

emptyMetrics :: Metrics
emptyMetrics = Metrics 0 0

addMetrics :: Metrics -> Metrics -> Metrics
addMetrics (Metrics a1 b1) (Metrics a2 b2) = Metrics (a1 + a2) (b1 + b2)

type SectionMap = Map Section Metrics

declLines :: SrcSpanInfo -> Int
declLines (SrcSpanInfo (SrcSpan _ sl _ el _) _) = el - sl + 1

classifyDecl :: Decl SrcSpanInfo -> Section
classifyDecl decl = case decl of
  TypeSig{} -> STypeSignatures
  FunBind{} -> SFunctionBindings
  PatBind{} -> SFunctionBindings
  DataDecl{} -> SDataDeclarations
  GDataDecl{} -> SDataDeclarations
  TypeDecl{} -> SDataDeclarations
  ClassDecl{} -> SClassInstances
  InstDecl{} -> SClassInstances
  DerivDecl{} -> SDerivingDeclarations
  SpliceDecl{} -> STemplateHaskell
  AnnPragma{} -> SPragmas
  _ -> SOther

classifyComment :: Comment -> Section
classifyComment _ = SComments

data FileMetrics = FileMetrics
  { fmFile :: String
  , fmTotalLines :: Int
  , fmSections :: SectionMap
  , fmTopBindings :: Int
  , fmPragmaLines :: Int
  }

analyzeFile :: FilePath -> IO FileMetrics
analyzeFile path = do
  rawContents <- readFile path
  let contents = preprocessSource rawContents
      totalLines = length (lines rawContents)

      mode =
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
                   ]
          , fixities = Nothing
          }

      result = parseFileContentsWithComments mode contents

  case result of
    ParseFailed loc msg ->
      error $ "Parse failed at " ++ show loc ++ ": " ++ msg
    ParseOk (Module _ mhead pragmaList importList decls, comments) -> do
      let pragmaMetrics = foldl' (\acc p -> addToMap SPragmas (pragmaLines p) acc) Map.empty pragmaList
          headMetrics = case mhead of
            Just (HSE.ModuleHead l _ _ _) -> addToMap SModuleHead (declLines l) pragmaMetrics
            Nothing -> pragmaMetrics
          importMetrics = foldl' (\acc (ImportDecl l _ _ _ _ _ _ _) -> addToMap SImports (declLines l) acc) headMetrics importList
          declMetrics = foldl' (\acc d -> addToMap (classifyDecl d) (declDeclLines d) acc) importMetrics decls
          commentMetrics = foldl' (\acc c -> addToMap (classifyComment c) (commentLines c) acc) declMetrics comments
          topBinds = countTopBindings decls
      return
        FileMetrics
          { fmFile = takeFileName path
          , fmTotalLines = totalLines
          , fmSections = commentMetrics
          , fmTopBindings = topBinds
          , fmPragmaLines = sum [pragmaLines p | p <- pragmaList]
          }
    ParseOk _ ->
      error $ "Unexpected parse result for " ++ path ++ " (not a Module)"

pragmaLines :: ModulePragma SrcSpanInfo -> Int
pragmaLines (LanguagePragma l _) = declLines l
pragmaLines (OptionsPragma l _ _) = declLines l
pragmaLines (AnnModulePragma l _) = declLines l

declDeclLines :: Decl SrcSpanInfo -> Int
declDeclLines d = case ann d of
  l -> declLines l

commentLines :: Comment -> Int
commentLines (Comment _ (SrcSpan _ sl _ el _) _) = el - sl + 1

addToMap :: Section -> Int -> SectionMap -> SectionMap
addToMap section lns = Map.insertWith addMetrics section (Metrics lns 1)

countTopBindings :: [Decl SrcSpanInfo] -> Int
countTopBindings = length . filter isFunOrPat
 where
  isFunOrPat FunBind{} = True
  isFunOrPat PatBind{} = True
  isFunOrPat _ = False

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
    | otherwise = stripTypeApplications line

  stripTypeApplications [] = []
  stripTypeApplications (' ' : '@' : rest) = ' ' : stripTypeApp rest
  stripTypeApplications ('@' : '(' : rest) = stripParens 1 rest
  stripTypeApplications (c : '@' : rest)
    | isIdChar c = c : stripTypeApp rest
  stripTypeApplications (c : cs) = c : stripTypeApplications cs

  isIdChar ch = ch `elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "'_#)")

  stripTypeApp [] = []
  stripTypeApp s@(c : cs)
    | c == '(' = stripParens 1 cs
    | isUpper c =
        let (name, rest) = span (\x -> isIdChar x || x == '.') s
         in stripTypeApplications rest
    | otherwise = stripTypeApplications s

  isUpper c = c `elem` ['A' .. 'Z']

  stripParens :: Int -> String -> String
  stripParens 0 s = stripTypeApplications s
  stripParens _ [] = []
  stripParens n ('(' : cs) = stripParens (n + 1) cs
  stripParens n (')' : cs) = stripParens (n - 1) cs
  stripParens n (_ : cs) = stripParens n cs

allSections :: [Section]
allSections =
  [ SPragmas
  , SModuleHead
  , SImports
  , STypeSignatures
  , SFunctionBindings
  , SDataDeclarations
  , SClassInstances
  , SDerivingDeclarations
  , STemplateHaskell
  , SComments
  , SOther
  ]

getMetrics :: Section -> SectionMap -> Metrics
getMetrics s m = Map.findWithDefault emptyMetrics s m

pad :: Int -> String -> String
pad n s = s ++ replicate (max 0 (n - length s)) ' '

rpad :: Int -> String -> String
rpad n s = replicate (max 0 (n - length s)) ' ' ++ s

data ScriptPair = ScriptPair
  { spName :: String
  , spPlutarchPath :: FilePath
  , spPlinthPath :: FilePath
  , spTypesPath :: Maybe FilePath
  }

allScriptPairs :: [ScriptPair]
allScriptPairs =
  [ ScriptPair
      "CIP-143 ProgrammableLogicBase"
      "src/SmartTokens/Contracts/ProgrammableLogicBase.hs"
      "src/SmartTokens/Contracts/ProgrammableLogicBasePlinth.hs"
      (Just "src/SmartTokens/Types/ProgrammableLogicGlobal.hs")
  , ScriptPair
      "Hydra Head Validator"
      "src/Hydra/Contracts/Head.hs"
      "src/Hydra/Contracts/HeadPlinth.hs"
      (Just "src/Hydra/Types/HeadState.hs")
  , ScriptPair
      "Constitution Sorted Validator"
      "src/Constitution/Contracts/ConstitutionSorted.hs"
      "src/Constitution/Contracts/ConstitutionSortedPlinth.hs"
      (Just "src/Constitution/Types/ConstitutionConfig.hs")
  ]

printComparison :: ScriptPair -> IO ()
printComparison ScriptPair{..} = do
  pm <- analyzeFile spPlutarchPath
  tm <- analyzeFile spPlinthPath

  putStrLn ""
  putStrLn $ "Code Size Comparison: " ++ spName
  putStrLn (replicate 105 '=')
  printf
    "  %-24s | %8s %6s | %8s %6s | %8s\n"
    ("Section" :: String)
    ("lines" :: String)
    ("decls" :: String)
    ("lines" :: String)
    ("decls" :: String)
    ("line ratio" :: String)
  printf
    "  %-24s | %8s %6s | %8s %6s | %8s\n"
    ("" :: String)
    ("Plutarch" :: String)
    ("" :: String)
    ("Plinth" :: String)
    ("" :: String)
    ("Pl/Pa" :: String)
  putStrLn (replicate 105 '-')

  let printRow section = do
        let Metrics pLines pCount = getMetrics section (fmSections pm)
            Metrics tLines tCount = getMetrics section (fmSections tm)
            ratio :: String
            ratio
              | pLines > 0 = printf "%.2fx" (fromIntegral tLines / fromIntegral pLines :: Double)
              | tLines > 0 = "N/A"
              | otherwise = "-"
        printf
          "  %-24s | %8d %6d | %8d %6d | %8s\n"
          (sectionLabel section)
          pLines
          pCount
          tLines
          tCount
          ratio

  mapM_ printRow allSections

  putStrLn (replicate 105 '-')

  let pTotal = fmTotalLines pm
      tTotal = fmTotalLines tm
      totalRatio = printf "%.2fx" (fromIntegral tTotal / fromIntegral pTotal :: Double) :: String
  printf
    "  %-24s | %8d        | %8d        | %8s\n"
    ("TOTAL" :: String)
    pTotal
    tTotal
    totalRatio

  putStrLn (replicate 105 '=')
  printf
    "  Top-level bindings:  Plutarch = %d,  Plinth = %d\n"
    (fmTopBindings pm)
    (fmTopBindings tm)

  case spTypesPath of
    Just typesPath -> do
      typesMetrics <- analyzeFile typesPath
      printf
        "  Shared types module:  %s  (%d lines, %d top-level bindings)\n"
        (takeFileName typesPath)
        (fmTotalLines typesMetrics)
        (fmTopBindings typesMetrics)
    Nothing -> pure ()

  putStrLn ""

printSummary :: [(String, FileMetrics, FileMetrics)] -> IO ()
printSummary pairs = do
  putStrLn "Summary: All Scripts"
  putStrLn (replicate 80 '=')
  printf
    "  %-34s | %8s | %8s | %8s\n"
    ("Script" :: String)
    ("Plutarch" :: String)
    ("Plinth" :: String)
    ("ratio" :: String)
  putStrLn (replicate 80 '-')
  mapM_
    ( \(name, pm, tm) -> do
        let pTotal = fmTotalLines pm
            tTotal = fmTotalLines tm
            ratio = printf "%.2fx" (fromIntegral tTotal / fromIntegral pTotal :: Double) :: String
        printf
          "  %-34s | %8d | %8d | %8s\n"
          name
          pTotal
          tTotal
          ratio
    )
    pairs
  putStrLn (replicate 80 '=')
  let totalP = sum [fmTotalLines pm | (_, pm, _) <- pairs]
      totalT = sum [fmTotalLines tm | (_, _, tm) <- pairs]
      totalRatio = printf "%.2fx" (fromIntegral totalT / fromIntegral totalP :: Double) :: String
  printf
    "  %-34s | %8d | %8d | %8s\n"
    ("TOTAL" :: String)
    totalP
    totalT
    totalRatio
  putStrLn ""

main :: IO ()
main = do
  mapM_ printComparison allScriptPairs

  summaryData <-
    mapM
      ( \ScriptPair{..} -> do
          pm <- analyzeFile spPlutarchPath
          tm <- analyzeFile spPlinthPath
          pure (spName, pm, tm)
      )
      allScriptPairs

  printSummary summaryData
