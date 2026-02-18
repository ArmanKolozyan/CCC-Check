{-# LANGUAGE OverloadedStrings #-}

-- | Comprehensive benchmarking suite for tagged Circomlib programs
--
-- This module benchmarks the value analysis performance on programs
-- from the tagged Circomlib library, using both:
--   1. Hand-built ASTs
--   2. IR files from the CirC pipeline
--
-- Usage examples:
--   cabal run taggedcircomlib-bench                    -- runs all benchmarks
--   cabal run taggedcircomlib-bench -- --list          -- lists all benchmarks
--   cabal run taggedcircomlib-bench -- --csv out.csv   -- saves results to CSV
--   cabal run taggedcircomlib-bench -- --match prefix "Full Pipeline"

module Main (main) where

import Criterion.Main ( defaultMain, bench, bgroup, nf, Benchmark )
import Control.DeepSeq (deepseq, NFData(..))
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import System.Directory (doesFileExist)

-- Importing all 20 programs
import qualified ValueInference.BugDetection.TaggedCircomlib.NotTest as NotTest
import qualified ValueInference.BugDetection.TaggedCircomlib.XorTest as XorTest
import qualified ValueInference.BugDetection.TaggedCircomlib.AndTest as AndTest
import qualified ValueInference.BugDetection.TaggedCircomlib.OrTest as OrTest
import qualified ValueInference.BugDetection.TaggedCircomlib.NandTest as NandTest
import qualified ValueInference.BugDetection.TaggedCircomlib.NorTest as NorTest
import qualified ValueInference.BugDetection.TaggedCircomlib.IsZeroTest as IsZeroTest
import qualified ValueInference.BugDetection.TaggedCircomlib.DecoderTestRaw as DecoderTestRaw
import qualified ValueInference.BugDetection.TaggedCircomlib.IsEqualTest as IsEqualTest
import qualified ValueInference.BugDetection.TaggedCircomlib.Num2BitsTest as Num2BitsTest
import qualified ValueInference.BugDetection.TaggedCircomlib.Bits2NumTest as Bits2NumTest
import qualified ValueInference.BugDetection.TaggedCircomlib.MultiMux1Test as MultiMux1Test
import qualified ValueInference.BugDetection.TaggedCircomlib.Mux1Test as Mux1Test
import qualified ValueInference.BugDetection.TaggedCircomlib.LessThanTest as LessThanTest
import qualified ValueInference.BugDetection.TaggedCircomlib.GreaterThanTest as GreaterThanTest
import qualified ValueInference.BugDetection.TaggedCircomlib.GreaterEqThanTest as GreaterEqThanTest
import qualified ValueInference.BugDetection.TaggedCircomlib.LessEqThanTest as LessEqThanTest
import qualified ValueInference.BugDetection.TaggedCircomlib.BigLessThanTest as BigLessThanTest
import qualified ValueInference.BugDetection.TaggedCircomlib.BinSubTest as BinSubTest
import qualified ValueInference.BugDetection.TaggedCircomlib.BinSumTest as BinSumTest

-- Importing the analysis functions
import ValueAnalysis.Analysis (analyzeProgram, analyzeProgramFull, precomputeAnalysis, runAnalysis, PrecomputedAnalysis(..))
import ValueAnalysis.VariableState (VariableState)
import Syntax.AST (Program(..), Tag, Binding(..), name, tag)
import Syntax.Compiler (parseAndCompile)
import Syntax.TagsParser (parseTagsFile)
import BugDetection.BugDetection (detectBugsWithStore)

-- All test programs with their names (hand-built ASTs)
testPrograms :: [(String, Program)]
testPrograms =
  [ ("NotTest", NotTest.notTestProgram)
  , ("XorTest", XorTest.xorTestProgram)
  , ("AndTest", AndTest.andTestProgram)
  , ("OrTest", OrTest.orTestProgram)
  , ("NandTest", NandTest.nandTestProgram)
  , ("NorTest", NorTest.norTestProgram)
  , ("IsZeroTest", IsZeroTest.isZeroTestProgram)
  , ("DecoderTestRaw", DecoderTestRaw.decoderTestRawProgram)
  , ("IsEqualTest", IsEqualTest.isEqualTestProgram)
  , ("Num2BitsTest", Num2BitsTest.num2BitsTestProgram)
  , ("Bits2NumTest", Bits2NumTest.bits2NumTestProgram)
  , ("MultiMux1Test", MultiMux1Test.multiMux1TestProgram)
  , ("Mux1Test", Mux1Test.mux1TestProgram)
  , ("LessThanTest", LessThanTest.lessThanTestProgram)
  , ("GreaterThanTest", GreaterThanTest.greaterThanTestProgram)
  , ("GreaterEqThanTest", GreaterEqThanTest.greaterEqThanTestProgram)
  , ("LessEqThanTest", LessEqThanTest.lessEqThanTestProgram)
  , ("BigLessThanTest", BigLessThanTest.bigLessThanTestProgram)
  , ("BinSubTest", BinSubTest.binSubTestProgram)
  , ("BinSumTest", BinSumTest.sumTestProgram)
  ]

-- All e2e programs (loaded from IR files)
e2ePrograms :: [String]
e2ePrograms =
  [ "and", "or", "not", "nand", "nor", "xor"
  , "check_bitify", "check_comparators"
  , "decoder"
  , "mux1", "mux11", "mux2", "mux21", "mux3", "mux31", "mux4", "mux41"
  , "binsub", "binsum"
  , "aliascheck", "sign", "constants"
  , "bigadd15", "bigadd23", "bigadd2030"
  , "bigsub23", "bigsub15"
  , "bigmult21", "bigmult22", "bigmult23"
  , "bigsubmodp_32"
  , "bigmod_32", "bigmod_22"
  , "escalarmul_min_test", "escalarmul_test"
  , "escalarmulfix_test", "escalarmulany_test"
  , "pedersen_test", "pedersen2_test"
  , "pointbits_loopback"
  , "babypbk_test"
  ]

-- | Applies tags from a tags map to a Program's bindings
applyTags :: Program -> Map.Map String Tag -> Program
applyTags prog tagsMap = prog
    { inputs = map applyTag (inputs prog)
    , computationVars = map applyTag (computationVars prog)
    , constraintVars = map applyTag (constraintVars prog)
    , returnVars = map applyTag (returnVars prog)
    }
  where
    applyTag binding = case Map.lookup (name binding) tagsMap of
        Just t  -> binding { tag = Just t }
        Nothing -> binding

-- | Loads a program from IR + tags files
loadE2EProgram :: String -> IO (Maybe Program)
loadE2EProgram progName = do
    let irDir = "/tmp/e2e_circom_test"
        circirFile = irDir ++ "/" ++ progName ++ ".circir"
        tagsFile = irDir ++ "/" ++ progName ++ ".tags"
    irExists <- doesFileExist circirFile
    tagsExists <- doesFileExist tagsFile
    if not irExists || not tagsExists
      then return Nothing
      else do
        content <- readFile circirFile
        case parseAndCompile content of
          Left _ -> return Nothing
          Right program -> do
            tagsResult <- parseTagsFile tagsFile
            case tagsResult of
              Left _ -> return Nothing
              Right tagsMap -> do
                let tagged = applyTags program tagsMap
                tagged `deepseq` return (Just tagged)

-- | Full pipeline: analysis + bug detection (what we compare against CIVER)
--   Uses precomputed data to avoid rebuilding nameToID every call.
runFullPipeline :: PrecomputedAnalysis -> Program -> (IntMap VariableState, Either [String] ())
runFullPipeline pa prog =
  let intMapStore = runAnalysis pa
      bugResult = detectBugsWithStore prog Nothing (paNameToID pa) intMapStore
  in intMapStore `deepseq` bugResult `deepseq` (intMapStore, bugResult)

-- creating benchmarks for hand-built programs (analysis only)
createBenchmarks :: [(String, Program)] -> [Benchmark]
createBenchmarks = map createBench
  where
    createBench (nm, program) = bench nm $ nf analyzeProgram program

-- creating benchmarks for e2e programs (full pipeline: analysis + bug detection)
-- Precomputes nameToID etc. once; benchmark only measures worklist + bug detection.
createE2EBenchmark :: String -> Program -> Benchmark
createE2EBenchmark nm prog =
    let pa = precomputeAnalysis prog
    in pa `deepseq` bench nm (nf (runFullPipeline pa) prog)

main :: IO ()
main = do
  -- loading all e2e programs upfront (parsing excluded from benchmark)
  e2eLoaded <- mapM (\nm -> do
      mp <- loadE2EProgram nm
      return (nm, mp)
    ) e2ePrograms
  let e2eBenches = [ createE2EBenchmark nm prog
                   | (nm, Just prog) <- e2eLoaded ]
  let missingCount = length [() | (_, Nothing) <- e2eLoaded]
  if missingCount > 0
    then putStrLn $ "Warning: " ++ show missingCount ++ " e2e programs not found (run run_e2e_circom.sh first)"
    else return ()

  defaultMain
    [ bgroup "Analysis Only (hand-built AST)" (createBenchmarks testPrograms)
    , bgroup "Full Pipeline (IR files)" e2eBenches
    ]
