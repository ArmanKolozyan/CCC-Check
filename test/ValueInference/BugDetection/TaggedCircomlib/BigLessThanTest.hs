{-# LANGUAGE OverloadedStrings #-}

module ValueInference.BugDetection.TaggedCircomlib.BigLessThanTest (spec, bigLessThanTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (fromLeft)
import Data.List (isInfixOf)

-- | BigLessThan template test program with 2-bit limbs and 2 limbs
bigLessThanTestProgram :: Program
bigLessThanTestProgram = 
  let
    
    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- template parameters
    n = 2 -- using 2-bit limbs for this test
    k = 2 -- using 2 limbs (so we're comparing 2 * 2-bit numbers)

    -- bindings

    -- inputs (two arrays of field values with maxbit constraint)
    a0 = Binding { name = "a0", vid = 0, sort = FieldMod p, tag = Just $ MaxValTag 3 } -- 2^2 - 1 = 3
    a1 = Binding { name = "a1", vid = 1, sort = FieldMod p, tag = Just $ MaxValTag 3 }
    b0 = Binding { name = "b0", vid = 2, sort = FieldMod p, tag = Just $ MaxValTag 3 }
    b1 = Binding { name = "b1", vid = 3, sort = FieldMod p, tag = Just $ MaxValTag 3 }

    -- LessThan components for each limb pair
    -- LessThan(n) for a[0] vs b[0]
    lt0_diff = Binding { name = "lt0_diff", vid = 4, sort = FieldMod p, tag = Just $ MaxValTag 7 } -- a[0] + 4 - b[0]
    lt0_diff_b0 = Binding { name = "lt0_diff_b0", vid = 5, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    lt0_diff_b1 = Binding { name = "lt0_diff_b1", vid = 6, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    lt0_diff_b2 = Binding { name = "lt0_diff_b2", vid = 7, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    lt0_out = Binding { name = "lt0_out", vid = 8, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- LessThan(n) for a[1] vs b[1]
    lt1_diff = Binding { name = "lt1_diff", vid = 9, sort = FieldMod p, tag = Just $ MaxValTag 7 }
    lt1_diff_b0 = Binding { name = "lt1_diff_b0", vid = 10, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    lt1_diff_b1 = Binding { name = "lt1_diff_b1", vid = 11, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    lt1_diff_b2 = Binding { name = "lt1_diff_b2", vid = 12, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    lt1_out = Binding { name = "lt1_out", vid = 13, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- IsEqual components for each limb pair
    eq0_diff = Binding { name = "eq0_diff", vid = 14, sort = FieldMod p, tag = Nothing }
    eq0_inv = Binding { name = "eq0_inv", vid = 15, sort = FieldMod p, tag = Nothing }
    eq0_out = Binding { name = "eq0_out", vid = 16, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    eq1_diff = Binding { name = "eq1_diff", vid = 17, sort = FieldMod p, tag = Nothing }
    eq1_inv = Binding { name = "eq1_inv", vid = 18, sort = FieldMod p, tag = Nothing }
    eq1_out = Binding { name = "eq1_out", vid = 19, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- ands[0] = eq[1] && lt[0]
    ands0_out = Binding { name = "ands0_out", vid = 20, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    -- ors[0] = lt[1] || ands[0]
    out = Binding { name = "out", vid = 21, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- constraints

    -- LessThan constraints for lt[0]: a[0] < b[0]
    c_lt0_diff = EqC 100 (Var "lt0_diff") (Sub (Add (Var "a0") (Int 4)) (Var "b0"))
    c_lt0_diff_b0_binary = EqC 101 (Mul (Var "lt0_diff_b0") (Sub (Var "lt0_diff_b0") (Int 1))) (Int 0)
    c_lt0_diff_b1_binary = EqC 102 (Mul (Var "lt0_diff_b1") (Sub (Var "lt0_diff_b1") (Int 1))) (Int 0)
    c_lt0_diff_b2_binary = EqC 103 (Mul (Var "lt0_diff_b2") (Sub (Var "lt0_diff_b2") (Int 1))) (Int 0)
    c_lt0_reconstruction = EqC 104 
          (Add (Add (Var "lt0_diff_b0") (Mul (Int 2) (Var "lt0_diff_b1"))) (Mul (Int 4) (Var "lt0_diff_b2")))
          (Var "lt0_diff")
    c_lt0_output = EqC 105 (Var "lt0_out") (Sub (Int 1) (Var "lt0_diff_b2"))

    -- LessThan constraints for lt[1]: a[1] < b[1]
    c_lt1_diff = EqC 106 (Var "lt1_diff") (Sub (Add (Var "a1") (Int 4)) (Var "b1"))
    c_lt1_diff_b0_binary = EqC 107 (Mul (Var "lt1_diff_b0") (Sub (Var "lt1_diff_b0") (Int 1))) (Int 0)
    c_lt1_diff_b1_binary = EqC 108 (Mul (Var "lt1_diff_b1") (Sub (Var "lt1_diff_b1") (Int 1))) (Int 0)
    c_lt1_diff_b2_binary = EqC 109 (Mul (Var "lt1_diff_b2") (Sub (Var "lt1_diff_b2") (Int 1))) (Int 0)
    c_lt1_reconstruction = EqC 110 
          (Add (Add (Var "lt1_diff_b0") (Mul (Int 2) (Var "lt1_diff_b1"))) (Mul (Int 4) (Var "lt1_diff_b2")))
          (Var "lt1_diff")
    c_lt1_output = EqC 111 (Var "lt1_out") (Sub (Int 1) (Var "lt1_diff_b2"))

    -- IsEqual constraints for eq[0]: a[0] == b[0]
    c_eq0_diff = EqC 112 (Var "eq0_diff") (Sub (Var "a0") (Var "b0"))
    c_eq0_iszero = EqC 113 (Var "eq0_out") (Add (Mul (Int (-1)) (Mul (Var "eq0_diff") (Var "eq0_inv"))) (Int 1))
    c_eq0_correctness = EqC 114 (Mul (Var "eq0_diff") (Var "eq0_out")) (Int 0)

    -- IsEqual constraints for eq[1]: a[1] == b[1]
    c_eq1_diff = EqC 115 (Var "eq1_diff") (Sub (Var "a1") (Var "b1"))
    c_eq1_iszero = EqC 116 (Var "eq1_out") (Add (Mul (Int (-1)) (Mul (Var "eq1_diff") (Var "eq1_inv"))) (Int 1))
    c_eq1_correctness = EqC 117 (Mul (Var "eq1_diff") (Var "eq1_out")) (Int 0)

    -- ands[0] = eq[1] && lt[0] = eq[1] * lt[0]
    c_ands0 = EqC 118 (Var "ands0_out") (Mul (Var "eq1_out") (Var "lt0_out"))

    -- out = lt[1] || ands[0] = lt[1] + ands[0] - lt[1] * ands[0]
    c_output = EqC 119 (Var "out") (Sub (Add (Var "lt1_out") (Var "ands0_out")) (Mul (Var "lt1_out") (Var "ands0_out")))

    -- all constraints
    allConstraints = [ 
          c_lt0_diff, c_lt0_diff_b0_binary, c_lt0_diff_b1_binary, c_lt0_diff_b2_binary, c_lt0_reconstruction, c_lt0_output,
          c_lt1_diff, c_lt1_diff_b0_binary, c_lt1_diff_b1_binary, c_lt1_diff_b2_binary, c_lt1_reconstruction, c_lt1_output,
          c_eq0_diff, c_eq0_iszero, c_eq0_correctness,
          c_eq1_diff, c_eq1_iszero, c_eq1_correctness,
          c_ands0, c_output ]

  in Program
      { inputs          = [a0, a1, b0, b1] -- two big integer inputs (each with k limbs)
      , computationVars = []
      , constraintVars  = [ lt0_diff, lt0_diff_b0, lt0_diff_b1, lt0_diff_b2, lt0_out,
                           lt1_diff, lt1_diff_b0, lt1_diff_b1, lt1_diff_b2, lt1_out,
                           eq0_diff, eq0_inv, eq0_out,
                           eq1_diff, eq1_inv, eq1_out,
                           ands0_out, out ]
      , computations    = []
      , constraints     = allConstraints
      , pfRecipExpressions = []
      , returnVars = [out] -- expected binary output
      }

spec :: Spec
spec = describe "BigLessThan Template Test" $ do
  it "detects expected analysis limitations for BigLessThan template" $ do
    -- running the bug detection
    let bugResult = detectBugs bigLessThanTestProgram Nothing

    let errors = fromLeft [] bugResult
    let errorString = unlines errors

    -- The analysis cannot deduce that several variables are binary due to cascading limitations
    -- with the IsEqual components embedded in the BigLessThan template:
    --
    -- 1. eq0_out and eq1_out: These are outputs from IsEqual components, which internally use
    --    IsZero logic (out = -diff*inv + 1, diff*out = 0). The analysis lacks symbolic reasoning
    --    to perform case analysis (if diff=0 vs diff≠0) and cannot combine the implications
    --    symbolically to deduce these outputs must be 0 or 1.
    --
    -- 2. ands0_out: This variable represents eq1_out && lt0_out. Since eq1_out is not recognized
    --    as binary, the Boolean AND rule doesn't trigger, so ands0_out is not constrained to be binary.
    --
    -- 3. out: The final output represents lt1_out || ands0_out. Since ands0_out is not recognized
    --    as binary, the Boolean OR rule doesn't trigger, so out is not constrained to be binary.

    -- assertions
    errorString `shouldSatisfy` ("Boolean variable `eq0_out`" `isInfixOf`)
    errorString `shouldSatisfy` ("Boolean variable `eq1_out`" `isInfixOf`)
    errorString `shouldSatisfy` ("Boolean variable `ands0_out`" `isInfixOf`)
    errorString `shouldSatisfy` ("Boolean variable `out`" `isInfixOf`)