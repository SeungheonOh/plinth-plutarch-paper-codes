{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Counts error-message jargon in src-errormsg-top8/EXX-*/{Plinth,Plutarch}.err.
--
-- DSL-jargon tokens reuse the curated lists from JargonLists (identical to the
-- source-program jargon methodology). Internal-jargon tokens are matched
-- against a separate pattern set covering TH skolems, Core variable suffixes,
-- class dictionaries, qualified GHC/PlutusTx-internal modules, Core
-- annotations, and Plinth-plugin banner phrases.
--
-- A single token receives at most one classification; Internal wins over DSL
-- because the paper explicitly says compiler-internal identifiers are worse.
--
-- Counts are summed across every diagnostic in the .err file; tokens inside
-- the GHC source-preview band are included.
module Main (main) where

import Control.Monad (forM, when)
import Data.Char (isAlpha, isAlphaNum, isDigit, isUpper)
import Data.List (foldl', isPrefixOf, sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import JargonLists
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- ============================================================================
-- 1. Internal jargon kinds
-- ============================================================================

data InternalKind
  = Banner
  | THSkolem
  | CoreVar
  | DictName
  | QualifiedInternal
  | CoreAnnot
  deriving (Eq, Ord, Show)

internalLabel :: InternalKind -> String
internalLabel Banner = "Plinth-plugin banners"
internalLabel THSkolem = "TH skolems"
internalLabel CoreVar = "Core variable suffixes"
internalLabel DictName = "Class dictionaries"
internalLabel QualifiedInternal = "Qualified internals"
internalLabel CoreAnnot = "Core annotations"

allInternalKinds :: [InternalKind]
allInternalKinds =
  [Banner, THSkolem, CoreVar, DictName, QualifiedInternal, CoreAnnot]

bannerPhrases :: [String]
bannerPhrases =
  [ "Plinth Compilation Error:"
  , "Context: Compiling code:"
  , "Error: Unsupported feature:"
  , "Reference to a name which is not a local, a builtin, or an external INLINABLE function"
  ]

internalQualPrefixes :: [String]
internalQualPrefixes =
  [ "GHC"
  , "PlutusTx.Builtins.Internal"
  , "PlutusTx.Numeric"
  ]

-- ============================================================================
-- 2. Token kinds
-- ============================================================================

data TokKind
  = TokInternal !InternalKind
  | TokDSL !Category
  | TokIgnored
  deriving (Eq, Show)

data Tok = Tok
  { tokText :: !String
  , tokKind :: !TokKind
  }
  deriving (Eq, Show)

-- ============================================================================
-- 3. Tokenizer
-- ============================================================================

tokenize :: Side -> String -> [Tok]
tokenize side = go
 where
  go [] = []
  go s
    | Just (matched, rest) <- matchBanner s =
        Tok matched (TokInternal Banner) : go rest
    | "[GHC-" `isPrefixOf` s =
        -- GHC error codes (e.g. [GHC-88464]) are explicitly excluded from
        -- the internal-jargon count. Consume the whole bracketed form
        -- without emitting any token.
        let afterDash = drop 5 s
            (_, rest0) = span isDigit afterDash
            rest = case rest0 of
              (']' : tl) -> tl
              _ -> rest0
         in go rest
    | "Occ=" `isPrefixOf` s =
        let after = drop 4 s
            (val, rest) = span isIdentChar after
            whole = "Occ=" ++ val
         in Tok whole (TokInternal CoreAnnot) : go rest
    | otherwise = case s of
        (c : cs)
          | c == '#' ->
              let (op, rest) = span isPlutarchOpChar cs
                  whole = c : op
               in classifyToken side whole : go rest
          | c == ':' && take 3 cs == "-->" ->
              classifyToken side ":-->" : go (drop 3 cs)
          | c == '$' && not (null cs) && head cs == 'f' ->
              let (ident, rest) = spanIdentChars cs
                  whole = '$' : ident
               in if null ident
                    then go cs
                    else Tok whole (TokInternal DictName) : go rest
          | isAlpha c || c == '_' ->
              let (ident, rest) = spanQualifiedIdent (c : cs)
               in classifyToken side ident : go rest
          | otherwise -> go cs

matchBanner :: String -> Maybe (String, String)
matchBanner s =
  case [p | p <- bannerPhrases, p `isPrefixOf` s] of
    (p : _) -> Just (p, drop (length p) s)
    [] -> Nothing

isPlutarchOpChar :: Char -> Bool
isPlutarchOpChar c = c `elem` ("#$=<>&|/" :: String)

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

spanIdentChars :: String -> (String, String)
spanIdentChars = span isIdentChar

-- Consume identifier possibly with dots (qualified name). A dot is consumed
-- only if followed by another identifier-start character.
spanQualifiedIdent :: String -> (String, String)
spanQualifiedIdent input = case spanIdentChars input of
  (ident, '.' : rest)
    | not (null rest)
    , let c = head rest
    , isAlpha c || c == '_' ->
        let (more, rest') = spanQualifiedIdent rest
         in (ident ++ "." ++ more, rest')
  (ident, rest) -> (ident, rest)

-- ============================================================================
-- 4. Token classification
-- ============================================================================

classifyToken :: Side -> String -> Tok
classifyToken side ident
  | isCoreAnnot ident = Tok ident (TokInternal CoreAnnot)
  | isQualifiedInternal ident = Tok ident (TokInternal QualifiedInternal)
  | isTHSkolem ident = Tok ident (TokInternal THSkolem)
  | isCoreVar ident = Tok ident (TokInternal CoreVar)
  | otherwise = case matchDSLByName (rulesFor side) ident of
      Just cat -> Tok ident (TokDSL cat)
      Nothing -> Tok ident TokIgnored

isCoreAnnot :: String -> Bool
isCoreAnnot ident = ident `elem` ["__DEFAULT", "OtherCon"]

isQualifiedInternal :: String -> Bool
isQualifiedInternal ident =
  any (\p -> ident == p || (p ++ ".") `isPrefixOf` ident) internalQualPrefixes

-- TH-generated skolem: starts uppercase, then "_" then >=6 consecutive digits.
isTHSkolem :: String -> Bool
isTHSkolem ident = case break (== '_') ident of
  (hd, '_' : tl)
    | not (null hd)
    , isUpper (head hd)
    , length tl >= 6
    , all isDigit tl ->
        True
  _ -> False

-- Core variable suffix: last underscore-separated segment matches "aXXX"
-- (alphanumeric, containing at least one digit) or "XNNN" (all digits).
isCoreVar :: String -> Bool
isCoreVar ident =
  case lastSegment ident of
    Nothing -> False
    Just lastSeg -> isCoreSuffix lastSeg
 where
  lastSegment s = case findLastUnderscore s of
    Just i -> Just (drop (i + 1) s)
    Nothing -> Nothing
  findLastUnderscore = go 0 Nothing
   where
    go _ acc [] = acc
    go n acc (c : cs)
      | c == '_' = go (n + 1) (Just n) cs
      | otherwise = go (n + 1) acc cs
  isCoreSuffix ('a' : rest@(_ : _)) = all isAlphaNum rest && any isDigit rest
  isCoreSuffix ('X' : rest@(_ : _)) = all isDigit rest
  isCoreSuffix _ = False

matchDSLByName :: [JargonRule] -> String -> Maybe Category
matchDSLByName rules ident
  | '.' `elem` ident =
      let (qual, base) = splitLastDot ident
          qualMatches = [c | QualPrefix p c <- rules, p == qual]
       in case qualMatches of
            (c : _) -> Just c
            [] -> exactMatch rules base
  | otherwise = exactMatch rules ident
 where
  exactMatch rs b =
    case [c | Exact n c <- rs, n == b] of
      (c : _) -> Just c
      [] -> Nothing

splitLastDot :: String -> (String, String)
splitLastDot s = case break (== '.') (reverse s) of
  (baseRev, '.' : qualRev) -> (reverse qualRev, reverse baseRev)
  _ -> ("", s)

-- ============================================================================
-- 5. Side selection
-- ============================================================================

data Side = PlinthSide | PlutarchSide
  deriving (Eq, Show)

sideLabel :: Side -> String
sideLabel PlinthSide = "Plinth"
sideLabel PlutarchSide = "Plutarch"

rulesFor :: Side -> [JargonRule]
rulesFor PlinthSide = plinthRules
rulesFor PlutarchSide = plutarchRules

-- ============================================================================
-- 6. Per-file analysis
-- ============================================================================

data ErrAnalysis = ErrAnalysis
  { eaCase :: !String
  , eaSide :: !Side
  , eaPath :: !FilePath
  , eaDSL :: !(Map Category Int)
  , eaInternal :: !(Map InternalKind Int)
  , eaDSLTotal :: !Int
  , eaInternalTotal :: !Int
  , eaLocations :: ![(String, Int, Int)]
  , eaDiagnosticCount :: !Int
  }
  deriving (Show)

analyzeFile :: String -> Side -> FilePath -> IO ErrAnalysis
analyzeFile caseName side path = do
  raw <- readFile path
  let toks = tokenize side raw
      dslMap = foldl' bumpDSL Map.empty toks
      internalMap = foldl' bumpInternal Map.empty toks
      dslTotal = sum (Map.elems dslMap)
      internalTotal = sum (Map.elems internalMap)
      locs = extractLocations path raw
      diagCount = countDiagnostics raw
  pure
    ErrAnalysis
      { eaCase = caseName
      , eaSide = side
      , eaPath = path
      , eaDSL = dslMap
      , eaInternal = internalMap
      , eaDSLTotal = dslTotal
      , eaInternalTotal = internalTotal
      , eaLocations = locs
      , eaDiagnosticCount = diagCount
      }
 where
  bumpDSL m (Tok _ (TokDSL c)) = Map.insertWith (+) c 1 m
  bumpDSL m _ = m
  bumpInternal m (Tok _ (TokInternal k)) = Map.insertWith (+) k 1 m
  bumpInternal m _ = m

-- Pull out every (file, line, col) reference. Used for line-score support.
extractLocations :: FilePath -> String -> [(String, Int, Int)]
extractLocations _errPath input = mapMaybe parseLoc (lines input)
 where
  parseLoc line = do
    let trimmed = dropWhile (`elem` (" \t" :: String)) line
    -- "<path>:<line>:<col>:" or "<path>:<line>:<col>-<col2>:"
    let parts = breakAll ':' trimmed
    case parts of
      (p : ls : cs : _)
        | all isDigit ls
        , not (null ls)
        , let startCol = takeWhile isDigit cs
        , not (null startCol)
        , looksLikePath p ->
            Just (p, read ls, read startCol)
      _ -> Nothing
  looksLikePath p = '/' `elem` p || ".hs" `isPrefixOf` reverse (take 3 (reverse p))
  breakAll _ [] = []
  breakAll c xs = case break (== c) xs of
    (a, _ : rest) -> a : breakAll c rest
    (a, []) -> [a]

-- Number of distinct GHC error-or-warning diagnostics in the file. Counts
-- lines that match either ":<digits>:<digits>:" followed by "error" or
-- "warning", or "Plinth Compilation Error:" or stand-alone "Error:" plugin
-- diagnostics.
countDiagnostics :: String -> Int
countDiagnostics input =
  let ls = lines input
      ghcDiag l =
        ("error:" `isInfixOf` l || "warning:" `isInfixOf` l)
          && hasGhcLocPrefix l
      pluginDiag l =
        "Plinth Compilation Error:" `isPrefixOf` l
      pluginErrorLine l =
        ("Error:" `isPrefixOf` l)
   in length (filter ghcDiag ls)
        + length (filter pluginDiag ls)
        + length (filter pluginErrorLine ls)
 where
  isInfixOf needle hay = any (needle `isPrefixOf`) (tails hay)
  tails [] = [[]]
  tails xss@(_ : xs) = xss : tails xs
  hasGhcLocPrefix l =
    case break (== ':') l of
      (p, ':' : rest)
        | not (null p) ->
            case break (== ':') rest of
              (ls', ':' : _)
                | not (null ls') && all isDigit ls' -> True
              _ -> False
      _ -> False

-- ============================================================================
-- 7. Cases
-- ============================================================================

topRoot :: FilePath
topRoot = "src-errormsg-top8"

topCases :: [String]
topCases =
  [ "E04-DivisionOnIntegers"
  , "E05-BoolForInteger"
  , "E07-WrongField"
  , "E12-MissingAppParens"
  , "E21-WrongPreludeFunction"
  , "E25-InfiniteType"
  , "E27-UnsupportedGADTs"
  , "E29-StageError"
  ]

discoverErrors :: IO [(String, Side, FilePath)]
discoverErrors = do
  rows <- forM topCases $ \caseName -> do
    let dir = topRoot </> caseName
    let plinthPath = dir </> "Plinth.err"
        plutarchPath = dir </> "Plutarch.err"
    plinthExists <- doesFileExist plinthPath
    plutarchExists <- doesFileExist plutarchPath
    let plinthEntry =
          [(caseName, PlinthSide, plinthPath) | plinthExists]
        plutarchEntry =
          [(caseName, PlutarchSide, plutarchPath) | plutarchExists]
    pure (plinthEntry ++ plutarchEntry)
  pure (concat rows)

-- ============================================================================
-- 8. Reporting
-- ============================================================================

printAnalysis :: ErrAnalysis -> IO ()
printAnalysis ErrAnalysis{..} = do
  putStrLn ""
  putStrLn $
    "=== "
      ++ eaCase
      ++ " / "
      ++ sideLabel eaSide
      ++ " ("
      ++ takeFileName eaPath
      ++ ") ==="
  printf "  Diagnostics in file: %d\n" eaDiagnosticCount
  printf "  DSL-jargon total:    %d\n" eaDSLTotal
  printf "  Internal total:      %d\n" eaInternalTotal
  putStrLn "  DSL breakdown:"
  let cats = case eaSide of
        PlinthSide -> plinthCategories
        PlutarchSide -> plutarchCategories
  mapM_
    ( \c ->
        printf
          "    %-32s  %d\n"
          (categoryLabel c)
          (Map.findWithDefault 0 c eaDSL)
    )
    cats
  putStrLn "  Internal breakdown:"
  mapM_
    ( \k ->
        printf
          "    %-32s  %d\n"
          (internalLabel k)
          (Map.findWithDefault 0 k eaInternal)
    )
    allInternalKinds
  when (not (null eaLocations)) $ do
    putStrLn "  Source locations (first 5):"
    mapM_
      ( \(p, l, c) ->
          printf
            "    %s:%d:%d\n"
            (takeFileName p)
            l
            c
      )
      (take 5 eaLocations)

renderCSV :: [ErrAnalysis] -> String
renderCSV rows =
  unlines (header : map row rows)
 where
  header =
    "case,side,diagnostics,dsl_total,internal_total,"
      ++ "dsl_plutarch_term,dsl_plutarch_op,dsl_plutarch_type,"
      ++ "dsl_plinth_term,dsl_plinth_builtin,dsl_plinth_container,dsl_plinth_type,"
      ++ "int_banner,int_th,int_corevar,int_dict,int_qual,int_annot,"
      ++ "primary_location"
  row ErrAnalysis{..} =
    let g c = Map.findWithDefault 0 c eaDSL
        i k = Map.findWithDefault 0 k eaInternal
        primary = case eaLocations of
          ((p, ln, col) : _) -> takeFileName p ++ ":" ++ show ln ++ ":" ++ show col
          [] -> ""
     in concat
          [ eaCase
          , ","
          , sideLabel eaSide
          , ","
          , show eaDiagnosticCount
          , ","
          , show eaDSLTotal
          , ","
          , show eaInternalTotal
          , ","
          , show (g PlutarchTermPrim)
          , ","
          , show (g PlutarchOperator)
          , ","
          , show (g PlutarchType)
          , ","
          , show (g PlinthTermPrim)
          , ","
          , show (g PlinthBuiltin)
          , ","
          , show (g PlinthDataContainer)
          , ","
          , show (g PlinthType)
          , ","
          , show (i Banner)
          , ","
          , show (i THSkolem)
          , ","
          , show (i CoreVar)
          , ","
          , show (i DictName)
          , ","
          , show (i QualifiedInternal)
          , ","
          , show (i CoreAnnot)
          , ","
          , primary
          ]

renderMarkdown :: [ErrAnalysis] -> String
renderMarkdown rows =
  unlines $
    [ "| Case | Side | Diagnostics | DSL | Internal | Primary location |"
    , "|------|------|------------:|----:|---------:|------------------|"
    ]
      ++ map renderRow rows
 where
  renderRow ErrAnalysis{..} =
    let primary = case eaLocations of
          ((p, ln, col) : _) ->
            takeFileName p ++ ":" ++ show ln ++ ":" ++ show col
          [] -> "—"
     in concat
          [ "| "
          , eaCase
          , " | "
          , sideLabel eaSide
          , " | "
          , show eaDiagnosticCount
          , " | "
          , show eaDSLTotal
          , " | "
          , show eaInternalTotal
          , " | `"
          , primary
          , "` |"
          ]

main :: IO ()
main = do
  errs <- discoverErrors
  rows <- forM errs $ \(caseName, side, path) -> analyzeFile caseName side path
  let ordered = sortBy (comparing (\r -> (eaCase r, sideLabel (eaSide r)))) rows
  mapM_ printAnalysis ordered
  putStrLn ""
  putStrLn "----- CSV -----"
  putStr (renderCSV ordered)
  putStrLn ""
  putStrLn "----- Markdown table -----"
  putStr (renderMarkdown ordered)
  when (null errs) $
    hPutStrLn stderr $
      "No .err files found under "
        ++ topRoot
        ++ " — check src-errormsg-top8/run.sh has been executed."
