{-# LANGUAGE OverloadedStrings #-}

-- | Comprehensive benchmarking suite for tagged Circomlib programs
-- 
-- This module benchmarks the value analysis performance on 20 different 
-- ZKP programs from the tagged Circomlib library.
--
-- Usage examples:
--   cabal run taggedcircomlib-bench                    -- runs all benchmarks
--   cabal run taggedcircomlib-bench -- --list          -- lists all benchmarks
--   cabal run taggedcircomlib-bench -- --csv out.csv   -- saves results to CSV

module Main (main) where

import Criterion.Main ( defaultMain, bench, bgroup, nf, Benchmark )

-- importing all 20 programs
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

-- importing the analysis function
import ValueAnalysis.Analysis (analyzeProgram)
import Syntax.AST (Program)

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

-- creating benchmarks for all programs
createBenchmarks :: [(String, Program)] -> [Benchmark]
createBenchmarks = map createBench
  where
    createBench (name, program) = bench name $ nf analyzeProgram program

main :: IO ()
main = defaultMain [
  bgroup "TaggedCircomlib ValueAnalysis" (createBenchmarks testPrograms)
  ]
