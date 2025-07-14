{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.GreaterThanTest (spec, greaterThanTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | GreaterThan template test program
greaterThanTestProgram :: Program
greaterThanTestProgram =
  let

    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- template parameter
    n = 2 -- using 2-bit comparison for this test

    -- bindings

    -- inputs (two field values with maxbit constraint)
    in0 = Binding { name = "in0", vid = 0, sort = FieldMod p, tag = Just $ MaxValTag 3 } -- 2^2 - 1 = 3
    in1 = Binding { name = "in1", vid = 1, sort = FieldMod p, tag = Just $ MaxValTag 3 } -- 2^2 - 1 = 3

    -- LessThan component intermediate signals (with swapped inputs: lt.in[0] = in[1], lt.in[1] = in[0])
    -- diff = in[1] + (1<<n) - in[0] = in[1] + 4 - in[0]
    -- max value is 3 + 4 - 0 = 7
    lt_diff = Binding { name = "lt_diff", vid = 2, sort = FieldMod p, tag = Just $ MaxValTag 7 }

    -- Num2Bits(n+1) = Num2Bits(3) component outputs for LessThan (binary bits of lt_diff)
    lt_diff_b0 = Binding { name = "lt_diff_b0", vid = 3, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    lt_diff_b1 = Binding { name = "lt_diff_b1", vid = 4, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    lt_diff_b2 = Binding { name = "lt_diff_b2", vid = 5, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- output (binary) - directly from LessThan component
    out = Binding { name = "out", vid = 6, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- constraints

    -- LessThan component logic with swapped inputs: lt_diff <== in[1] + (1<<n) - in[0] = in[1] + 4 - in[0]
    c_lt_diff = EqC 100 (Var "lt_diff") (Sub (Add (Var "in1") (Int 4)) (Var "in0"))

    -- Num2Bits constraints for lt_diff (binary constraints)
    c_lt_diff_b0_binary = EqC 101 (Mul (Var "lt_diff_b0") (Sub (Var "lt_diff_b0") (Int 1))) (Int 0)
    c_lt_diff_b1_binary = EqC 102 (Mul (Var "lt_diff_b1") (Sub (Var "lt_diff_b1") (Int 1))) (Int 0)
    c_lt_diff_b2_binary = EqC 103 (Mul (Var "lt_diff_b2") (Sub (Var "lt_diff_b2") (Int 1))) (Int 0)

    -- Num2Bits reconstruction: lt_diff_b0 + 2*lt_diff_b1 + 4*lt_diff_b2 === lt_diff
    c_lt_reconstruction = EqC 104 
          (Add (Add (Var "lt_diff_b0") (Mul (Int 2) (Var "lt_diff_b1"))) (Mul (Int 4) (Var "lt_diff_b2")))
          (Var "lt_diff")

    -- LessThan output: out <== 1 - lt_diff_b2 (where lt_diff_b2 is the highest bit)
    c_output = EqC 105 (Var "out") (Sub (Int 1) (Var "lt_diff_b2"))

    -- all constraints
    allConstraints = [ c_lt_diff, c_lt_diff_b0_binary, c_lt_diff_b1_binary, c_lt_diff_b2_binary, c_lt_reconstruction, c_output ]
    
  in Program
       { inputs          = [in0, in1] -- two field value inputs with maxbit constraints
       , computationVars = []
       , constraintVars  = [ lt_diff, lt_diff_b0, lt_diff_b1, lt_diff_b2, out ]
       , computations    = []
       , constraints     = allConstraints
       , pfRecipExpressions = []
       , returnVars = [out] -- expected binary output
       }

spec :: Spec
spec = describe "GreaterThan Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do

    -- running the bug detection
    let bugResult = detectBugs greaterThanTestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight