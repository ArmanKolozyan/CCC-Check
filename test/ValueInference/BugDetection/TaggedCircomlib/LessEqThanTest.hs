{-# LANGUAGE OverloadedStrings #-}

module ValueInference.BugDetection.TaggedCircomlib.LessEqThanTest (spec, lessEqThanTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | LessEqThan template test program with 2-bit comparison
lessEqThanTestProgram :: Program
lessEqThanTestProgram = 
  let
    
    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- template parameter
    n = 2 -- using 2-bit comparison for this test

    -- bindings

    -- inputs (two field values with maxbit constraint)
    in0 = Binding { name = "in0", vid = 0, sort = FieldMod p, tag = Just $ MaxValTag 3 } -- 2^2 - 1 = 3
    in1 = Binding { name = "in1", vid = 1, sort = FieldMod p, tag = Just $ MaxValTag 3 } -- 2^2 - 1 = 3

    -- GreaterThan component intermediate signals
    -- diff = in[1] + (1<<n) - in[0] = in[1] + 4 - in[0] (for GreaterThan logic)
    -- max value is 3 + 4 - 0 = 7
    gt_diff = Binding { name = "gt_diff", vid = 2, sort = FieldMod p, tag = Just $ MaxValTag 7 }

    -- Num2Bits(n+1) = Num2Bits(3) component outputs for GreaterThan (binary bits of gt_diff)
    gt_diff_b0 = Binding { name = "gt_diff_b0", vid = 3, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    gt_diff_b1 = Binding { name = "gt_diff_b1", vid = 4, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    gt_diff_b2 = Binding { name = "gt_diff_b2", vid = 5, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- GreaterThan output (binary)
    gt_out = Binding { name = "gt_out", vid = 6, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- final output (binary) - NOT of GreaterThan output
    out = Binding { name = "out", vid = 7, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- constraints

    -- GreaterThan component logic: gt_diff <== in[1] + (1<<n) - in[0] = in[1] + 4 - in[0]
    c_gt_diff = EqC 100 (Var "gt_diff") (Sub (Add (Var "in1") (Int 4)) (Var "in0"))

    -- Num2Bits constraints for gt_diff (binary constraints)
    c_gt_diff_b0_binary = EqC 101 (Mul (Var "gt_diff_b0") (Sub (Var "gt_diff_b0") (Int 1))) (Int 0)
    c_gt_diff_b1_binary = EqC 102 (Mul (Var "gt_diff_b1") (Sub (Var "gt_diff_b1") (Int 1))) (Int 0)
    c_gt_diff_b2_binary = EqC 103 (Mul (Var "gt_diff_b2") (Sub (Var "gt_diff_b2") (Int 1))) (Int 0)

    -- Num2Bits reconstruction: gt_diff_b0 + 2*gt_diff_b1 + 4*gt_diff_b2 === gt_diff
    c_gt_reconstruction = EqC 104 
          (Add (Add (Var "gt_diff_b0") (Mul (Int 2) (Var "gt_diff_b1"))) (Mul (Int 4) (Var "gt_diff_b2")))
          (Var "gt_diff")

    -- GreaterThan output: gt_out <== 1 - gt_diff_b2 (where gt_diff_b2 is the highest bit)
    c_gt_output = EqC 105 (Var "gt_out") (Sub (Int 1) (Var "gt_diff_b2"))

    -- NOT component logic: out <== 1 + gt_out - 2*gt_out
    c_not_output = EqC 106 (Var "out") (Sub (Add (Int 1) (Var "gt_out")) (Mul (Int 2) (Var "gt_out")))

    -- all constraints
    allConstraints = [ c_gt_diff, c_gt_diff_b0_binary, c_gt_diff_b1_binary, c_gt_diff_b2_binary, c_gt_reconstruction, c_gt_output, c_not_output ]

  in Program
      { inputs          = [in0, in1] -- two field value inputs with maxbit constraints
      , computationVars = []
      , constraintVars  = [ gt_diff, gt_diff_b0, gt_diff_b1, gt_diff_b2, gt_out, out ]
      , computations    = []
      , constraints     = allConstraints
      , pfRecipExpressions = []
      , returnVars = [out] -- expected binary output
      }

spec :: Spec
spec = describe "LessEqThan Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do
    -- running the bug detection
    let bugResult = detectBugs lessEqThanTestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight