{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.LessThanTest (spec, lessThanTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | LessThan template test program with 2-bit comparison
lessThanTestProgram :: Program
lessThanTestProgram = 
  let
    
    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- template parameter
    n = 2 -- using 2-bit comparison for this test

    -- bindings

    -- inputs (two field values with maxbit constraint)
    in0 = Binding { name = "in0", vid = 0, sort = FieldMod p, tag = Just $ MaxValTag 3 } -- 2^2 - 1 = 3
    in1 = Binding { name = "in1", vid = 1, sort = FieldMod p, tag = Just $ MaxValTag 3 } -- 2^2 - 1 = 3

    -- intermediate: diff = in[0] + (1<<n) - in[1] = in[0] + 4 - in[1]
    -- max value is 3 + 4 - 0 = 7
    diff = Binding { name = "diff", vid = 2, sort = FieldMod p, tag = Just $ MaxValTag 7 }

    -- Num2Bits(n+1) = Num2Bits(3) component outputs (binary bits of diff)
    diff_b0 = Binding { name = "diff_b0", vid = 3, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    diff_b1 = Binding { name = "diff_b1", vid = 4, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    diff_b2 = Binding { name = "diff_b2", vid = 5, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- output (binary)
    out = Binding { name = "out", vid = 6, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- constraints

    -- difference: diff <== in[0] + (1<<n) - in[1] = in[0] + 4 - in[1]
    c_diff = EqC 100 (Var "diff") (Sub (Add (Var "in0") (Int 4)) (Var "in1"))

    -- Num2Bits constraints for diff (binary constraints)
    c_diff_b0_binary = EqC 101 (Mul (Var "diff_b0") (Sub (Var "diff_b0") (Int 1))) (Int 0)
    c_diff_b1_binary = EqC 102 (Mul (Var "diff_b1") (Sub (Var "diff_b1") (Int 1))) (Int 0)
    c_diff_b2_binary = EqC 103 (Mul (Var "diff_b2") (Sub (Var "diff_b2") (Int 1))) (Int 0)

    -- Num2Bits reconstruction: diff_b0 + 2*diff_b1 + 4*diff_b2 === diff
    c_reconstruction = EqC 104 
          (Add (Add (Var "diff_b0") (Mul (Int 2) (Var "diff_b1"))) (Mul (Int 4) (Var "diff_b2")))
          (Var "diff")

    -- LessThan output: out <== 1 - diff_b2 (where diff_b2 is the highest bit)
    c_output = EqC 105 (Var "out") (Sub (Int 1) (Var "diff_b2"))

    -- all constraints
    allConstraints = [ c_diff, c_diff_b0_binary, c_diff_b1_binary, c_diff_b2_binary, c_reconstruction, c_output ]

  in Program
      { inputs          = [in0, in1] -- two field value inputs with maxbit constraints
      , computationVars = []
      , constraintVars  = [ diff, diff_b0, diff_b1, diff_b2, out ]
      , computations    = []
      , constraints     = allConstraints
      , pfRecipExpressions = []
      , returnVars = [out] -- expected binary output
      }

spec :: Spec
spec = describe "LessThan Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do
    -- running the bug detection
    let bugResult = detectBugs lessThanTestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight