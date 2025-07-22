{-# LANGUAGE OverloadedStrings #-}

-- | Variable Range Collector for Tagged Circomlib Programs
-- 
-- This module analyzes the variable ranges in 20 different ZKP programs 
-- from the tagged Circomlib library and writes the results to a file
-- in a format similar to the Picus range collector output.
--
-- Usage: cabal run taggedcircomlib-ranges

module Main (main) where

import System.IO (writeFile, appendFile)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Printf (printf)

-- importing all test programs
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.NotTest as NotTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.XorTest as XorTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.AndTest as AndTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.OrTest as OrTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.NandTest as NandTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.NorTest as NorTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.IsZeroTest as IsZeroTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.DecoderTestRaw as DecoderTestRaw
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.IsEqualTest as IsEqualTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.Num2BitsTest as Num2BitsTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.Bits2NumTest as Bits2NumTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.MultiMux1Test as MultiMux1Test
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.Mux1Test as Mux1Test
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.LessThanTest as LessThanTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.GreaterThanTest as GreaterThanTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.GreaterEqThanTest as GreaterEqThanTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.LessEqThanTest as LessEqThanTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.BigLessThanTest as BigLessThanTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.BinSubTest as BinSubTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.BinSumTest as BinSumTest

-- importing analysis modules
import ValueAnalysis.Analysis (analyzeProgram, VariableState(..))
import ValueAnalysis.ValueDomain (ValueDomain(..))
import ValueAnalysis.Printer (formatDomain)
import Syntax.AST (Program)

-- output file name
outputFile :: String
outputFile = "ranges_taggedcircomlib.txt"

-- all test programs with their names
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

-- BN254 field prime
bn254Prime :: Integer
bn254Prime = 21888242871839275222246405745257275088548364400416034343698204186575808495617

-- checks if a range covers the entire field (unrestricted)
isFullField :: ValueDomain -> Bool
isFullField (BoundedValues lbM ubM currentGaps) = 
  case (lbM, ubM) of
    (Just 0, Just high) -> high == bn254Prime - 1 && Set.null currentGaps
    _ -> False
isFullField (KnownValues s) = Set.null s  -- empty set is bottom/unrestricted
isFullField (ArrayDomain _ defaultDomain _) = isFullField defaultDomain

-- counts how many variables have restricted ranges
countRestrictedRanges :: Map.Map String VariableState -> Int
countRestrictedRanges states = 
  Map.size $ Map.filter (\state -> not (isFullField (domain state))) states

-- analyzes a single program and write results
analyzeAndWriteProgram :: String -> Program -> IO (Int, Int)
analyzeAndWriteProgram name program = do
  putStrLn $ "Processing: " ++ name
  
  let states = analyzeProgram program
  let restrictedCount = countRestrictedRanges states
  let totalVars = Map.size states
  
  appendFile outputFile $ printf "### Test: %s\n" name
  
  -- writing variable ranges in Picus-like format
  mapM_ writeVariableRange (Map.toList states)
  
  appendFile outputFile $ printf "Restricted variables: %d/%d\n" restrictedCount totalVars
  appendFile outputFile $ "\n" ++ replicate 80 '=' ++ "\n\n"
  
  return (restrictedCount, totalVars)
  where
    writeVariableRange (varName, state) = do
      let domainStr = formatDomain (domain state)
      appendFile outputFile $ printf "possible values for %s: %s\n" varName domainStr

-- main function
main :: IO ()
main = do
  putStrLn "Variable Range Analysis for Tagged Circomlib Programs"
  putStrLn $ "Output will be written to: " ++ outputFile
  
  -- initializing output file
  writeFile outputFile "Variable Range Analysis for Tagged Circomlib Programs\n"
  appendFile outputFile $ replicate 80 '=' ++ "\n\n"
  
  -- processing all test programs
  results <- mapM (uncurry analyzeAndWriteProgram) testPrograms
  
  let (restrictedCounts, totalCounts) = unzip results
  let totalRestricted = sum restrictedCounts
  let totalVariables = sum totalCounts
  
  -- writing summary
  appendFile outputFile $ "\n" ++ replicate 80 '=' ++ "\n"
  appendFile outputFile "SUMMARY\n"
  appendFile outputFile $ replicate 80 '=' ++ "\n"
  appendFile outputFile $ printf "Total tests processed: %d\n" (length testPrograms)
  appendFile outputFile $ printf "Total variables: %d\n" totalVariables
  appendFile outputFile $ printf "Total restricted variables: %d\n" totalRestricted
  
  if totalVariables > 0 then do
    let percentage = (fromIntegral totalRestricted / fromIntegral totalVariables) * 100 :: Double
    appendFile outputFile $ printf "Percentage of variables with restricted ranges: %.2f%%\n" percentage
  else
    appendFile outputFile "Percentage of variables with restricted ranges: N/A\n"
  
  putStrLn $ printf "Analysis complete! Processed %d test programs." (length testPrograms)
  putStrLn $ printf "Total restricted variables: %d/%d (%.2f%%)" 
    totalRestricted totalVariables 
    (if totalVariables > 0 then (fromIntegral totalRestricted / fromIntegral totalVariables) * 100 else 0 :: Double)
  putStrLn $ "Results saved to: " ++ outputFile
