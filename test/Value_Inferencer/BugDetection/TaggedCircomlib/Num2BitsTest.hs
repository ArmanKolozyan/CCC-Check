{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.Num2BitsTest (spec, num2BitsTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | Num2Bits template test program
num2BitsTestProgram :: Program
num2BitsTestProgram =
  let
    
    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- template parameter
    n = 3 -- using 3 bits for this test

    -- bindings

    -- input (field value that should be representable in n bits)
    inp = Binding { name = "in", vid = 0, sort = FieldMod p, tag = Just $ MaxValTag 7 } -- 2^3 - 1 = 7

    -- output bits (binary signals)
    out0 = Binding { name = "out0", vid = 1, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    out1 = Binding { name = "out1", vid = 2, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    out2 = Binding { name = "out2", vid = 3, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- constraints

    -- binary constraints for each output bit: out[i] * (out[i] - 1) === 0
    c_out0_binary = EqC 100 (Mul (Var "out0") (Sub (Var "out0") (Int 1))) (Int 0)
    c_out1_binary = EqC 101 (Mul (Var "out1") (Sub (Var "out1") (Int 1))) (Int 0)
    c_out2_binary = EqC 102 (Mul (Var "out2") (Sub (Var "out2") (Int 1))) (Int 0)

    -- reconstruction constraint: out0 + 2*out1 + 4*out2 === in
    c_reconstruction = EqC 103 
          (Add (Add (Var "out0") (Mul (Int 2) (Var "out1"))) (Mul (Int 4) (Var "out2")))
          (Var "in")

    -- all constraints
    allConstraints = [ c_out0_binary, c_out1_binary, c_out2_binary, c_reconstruction ]

  in Program
       { inputs          = [inp] -- input field value with max value constraint
       , computationVars = []
       , constraintVars  = [ out0, out1, out2 ]
       , computations    = []
       , constraints     = allConstraints
       , pfRecipExpressions = []
       , returnVars = [out0, out1, out2] -- expected binary outputs
       }

spec :: Spec
spec = describe "Num2Bits Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do

    -- running the bug detection
    let bugResult = detectBugs num2BitsTestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight