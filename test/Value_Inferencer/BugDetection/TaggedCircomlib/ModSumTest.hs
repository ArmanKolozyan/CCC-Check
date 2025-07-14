{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.ModSumTest (spec, modSumTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)
import Data.List (isInfixOf)

-- | ModSum template test program
modSumTestProgram :: Program
modSumTestProgram =
  let

    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- template parameter
    n = 3 -- using 3-bit modular addition for this test

    -- bindings

    -- inputs (two field values with maxbit constraint)
    a = Binding { name = "a", vid = 0, sort = FieldMod p, tag = Just $ MaxValTag 7 } -- 2^3 - 1 = 7
    b = Binding { name = "b", vid = 1, sort = FieldMod p, tag = Just $ MaxValTag 7 } -- 2^3 - 1 = 7

    -- intermediate calculation: a + b (can be up to 14)
    a_plus_b = Binding { name = "a_plus_b", vid = 2, sort = FieldMod p, tag = Just $ MaxValTag 14 }

    -- Num2Bits(n+1) = Num2Bits(4) component outputs (binary bits of a + b)
    sum_b0 = Binding { name = "sum_b0", vid = 3, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    sum_b1 = Binding { name = "sum_b1", vid = 4, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    sum_b2 = Binding { name = "sum_b2", vid = 5, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    sum_b3 = Binding { name = "sum_b3", vid = 6, sort = FieldMod p, tag = Just $ SimpleTag "binary" }

    -- outputs
    carry = Binding { name = "carry", vid = 7, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    sum_out = Binding { name = "sum", vid = 8, sort = FieldMod p, tag = Just $ MaxValTag 7 } -- maxbit = n

    -- constraints

    -- a + b
    c_a_plus_b = EqC 100 (Var "a_plus_b") (Add (Var "a") (Var "b"))

    -- Num2Bits constraints for a + b (binary constraints)
    c_sum_b0_binary = EqC 101 (Mul (Var "sum_b0") (Sub (Var "sum_b0") (Int 1))) (Int 0)
    c_sum_b1_binary = EqC 102 (Mul (Var "sum_b1") (Sub (Var "sum_b1") (Int 1))) (Int 0)
    c_sum_b2_binary = EqC 103 (Mul (Var "sum_b2") (Sub (Var "sum_b2") (Int 1))) (Int 0)
    c_sum_b3_binary = EqC 104 (Mul (Var "sum_b3") (Sub (Var "sum_b3") (Int 1))) (Int 0)

    -- Num2Bits reconstruction: sum_b0 + 2*sum_b1 + 4*sum_b2 + 8*sum_b3 === a + b
    c_reconstruction = EqC 105 
          (Add (Add (Add (Var "sum_b0") (Mul (Int 2) (Var "sum_b1"))) (Mul (Int 4) (Var "sum_b2"))) (Mul (Int 8) (Var "sum_b3")))
          (Var "a_plus_b")

    -- carry output: carry <== sum_b3 (the nth bit where n=3)
    c_carry = EqC 106 (Var "carry") (Var "sum_b3")

    -- sum output: sum <== a + b - carry * (1 << n) = a + b - carry * 8
    c_sum = EqC 107 (Var "sum") (Sub (Var "a_plus_b") (Mul (Var "carry") (Int 8)))

    -- all constraints
    allConstraints = [ c_a_plus_b, c_sum_b0_binary, c_sum_b1_binary, c_sum_b2_binary, c_sum_b3_binary, c_reconstruction, c_carry, c_sum ]
    
  in Program
       { inputs          = [a, b] -- two field value inputs with maxbit constraints
       , computationVars = []
       , constraintVars  = [ a_plus_b, sum_b0, sum_b1, sum_b2, sum_b3, carry, sum_out ]
       , computations    = []
       , constraints     = allConstraints
       , pfRecipExpressions = []
       , returnVars = [sum_out, carry] -- expected outputs
       }

spec :: Spec
spec = describe "ModSum Template Test" $ do
  it "detects analysis limitation with modular arithmetic bounds" $ do

    -- running the bug detection
    let bugResult = detectBugs modSumTestProgram Nothing

    -- assertions

    -- The analysis detects a potential violation because it cannot understand
    -- the interdependence between carry and the sum output.
    --
    -- WHAT THE MODSUM TEMPLATE ACTUALLY DOES:
    -- The ModSum template performs n-bit modular addition: given two n-bit numbers a and b,
    -- it computes their sum modulo 2^n. With n=3:
    -- - Input range: a, b ∈ [0, 7] (since 2^3 - 1 = 7)
    -- - The sum a + b can be in range [0, 14]
    -- - If a + b < 8 (i.e., < 2^3), then carry = 0 and sum = a + b
    -- - If a + b >= 8 (i.e., >= 2^3), then carry = 1 and sum = (a + b) - 8 = (a + b) mod 8
    -- - Therefore, sum is always in range [0, 7], which satisfies MaxValTag 7
    --
    -- WHY THE ANALYSIS FAILS:
    -- Our non-relational analysis examines the constraint sum = a_plus_b - carry * 8
    -- independently and sees:
    -- - a_plus_b can be up to 14
    -- - carry can be 0 or 1
    -- - So sum could be: 14 - 0*8 = 14 (when carry=0) or 14 - 1*8 = 6 (when carry=1)
    -- - The analysis conservatively assumes sum could be up to 14, violating MaxValTag 7
    --
    -- The analysis cannot understand that carry=1 exactly when a_plus_b >= 8,
    -- which would ensure sum <= 7 in all cases. This requires relational reasoning
    -- about the interdependence of variables.
    case bugResult of
      Left errors -> do
        -- we expect the error about sum having upper bound > 7
        (unlines errors) `shouldSatisfy` ("Variable `sum` has upper bound > 7" `isInfixOf`)
        
      Right _ -> 
        expectationFailure "Expected analysis to detect upper bound violation for sum variable"