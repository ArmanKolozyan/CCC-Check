{-# LANGUAGE OverloadedStrings #-}

module ValueInference.BugDetection.TaggedCircomlib.Bits2NumTest (spec, bits2NumTestProgram) where

import Test.Hspec ( describe, it, shouldSatisfy, Spec )
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | Bits2Num template test program
bits2NumTestProgram :: Program
bits2NumTestProgram = 
 let 
   
  -- using BN254 as prime field for demonstration
  p = 21888242871839275222246405745257275088548364400416034343698204186575808495617
      
  -- template parameter
  n = 3 -- using 3 bits for this test
 
  -- bindings
 
  -- input bits (binary signals)
  in0 = Binding { name = "in0", vid = 0, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
  in1 = Binding { name = "in1", vid = 1, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
  in2 = Binding { name = "in2", vid = 2, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
 
  -- output (field value with maxbit tag)
  out = Binding { name = "out", vid = 3, sort = FieldMod p, tag = Just $ MaxValTag 7 } -- 2^3 - 1 = 7
 
  -- constraints
 
   -- Bits2Num conversion constraint: out <== in0 + 2*in1 + 4*in2
  c_conversion = EqC 100 (Var "out") 
       (Add (Add (Var "in0") (Mul (Int 2) (Var "in1"))) (Mul (Int 4) (Var "in2")))
 
  -- all constraints
  allConstraints = [ c_conversion ]
 
  in Program
       { inputs          = [in0, in1, in2] -- binary input bits
       , computationVars = []
       , constraintVars  = [ out ]
       , computations    = []
       , constraints     = allConstraints
       , pfRecipExpressions = []
       , returnVars = [out] -- expected output with maxbit constraint
       }
 

spec :: Spec
spec = describe "Bits2Num Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do

    -- running the bug detection
    let bugResult = detectBugs bits2NumTestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight