{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

-- FIXED V-CIRCOMLIB-VUL-004 from Veridise Circomlib audit
module Value_Inferencer.Analysis.MontgomeryAddFixedTest (spec) where

import Test.Hspec
import Syntax.AST
import ValueAnalysis.Analysis
import Data.List (isInfixOf)
import Data.Either (fromLeft)

-- FIXED V-CIRCOMLIB-VUL-004 from Veridise Circomlib audit
spec :: Spec
spec = describe "Fixed MontgomeryAdd template test" $ do
  it "detects no division by zero errors" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- constants from the template
    let a = 168_700
    let d_ = 168_696
    let constA = 168_698
    let constB = 1

    -- original variables
    let in1_0 = Binding { name = "in1_0", vid = 0, sort = FieldMod p }
    let in1_1 = Binding { name = "in1_1", vid = 1, sort = FieldMod p }
    let in2_0 = Binding { name = "in2_0", vid = 2, sort = FieldMod p }
    let in2_1 = Binding { name = "in2_1", vid = 3, sort = FieldMod p }
    let out_0 = Binding { name = "out_0", vid = 4, sort = FieldMod p }
    let out_1 = Binding { name = "out_1", vid = 5, sort = FieldMod p }
    let lamda = Binding { name = "lamda", vid = 6, sort = FieldMod p }

    -- variables for IsZero instance checkZero (for denominator in2[0] - in1[0])
    let cz_in  = Binding { name = "cz_in",  vid = 10, sort = FieldMod p }
    let cz_out = Binding { name = "cz_out", vid = 11, sort = FieldMod p }
    let cz_inv = Binding { name = "cz_inv", vid = 12, sort = FieldMod p }

    -- computations
    let comp0 = Assign "lamda" (Mul (Sub (Var "in2_1") (Var "in1_1"))
                                     (PfRecip (Sub (Var "in2_0") (Var "in1_0"))))

    -- constraints

    -- constraints for checkZero instance (checks in2[0] - in1[0] != 0)
    let cz_assign_in = EqC 100 (Var "cz_in")  -- cz_in <= in2[0]- in1[0]
    let cz_c1 = EqC 101 (Add (Var "cz_out") (Mul (Var "cz_in") (Var "cz_inv"))) (Int 1)
    let cz_c2 = EqC 102 (Mul (Var "cz_in") (Var "cz_out")) (Int 0)
    let cz_force_out = EqC 103 (Var "cz_out") (Int 0) -- cz_out === 0 (enforces cz_in != 0)

    -- original MontgomeryAdd constraints
    let c1 = EqC 200 (Mul (Var "lamda") (Sub (Var "in2_0") (Var "in1_0"))) (Sub (Var "in2_1") (Var "in1_1"))
    let c2 = EqC 201 (Var "out_0")
                     (Sub (Sub (Sub (Mul (Int constB) (Mul (Var "lamda") (Var "lamda")))
                                    (Int constA))
                               (Var "in1_0"))
                          (Var "in2_0"))
    let c3 = EqC 202 (Var "out_1")
                     (Sub (Mul (Var "lamda") (Sub (Var "in1_0") (Var "out_0")))
                          (Var "in1_1"))

    -- combining all constraints
    let allConstraints = [ cz_assign_in, cz_c1, cz_c2, cz_force_out
                         , c1, c2, c3
                         ]

    -- expressions used in PfRecip (denominators)
    let denominators = [ (Sub (Var "in2_0") (Var "in1_0")) -- from comp0
                       ]

    -- the test program
    let testProgram = Program
          { inputs          = [in1_0, in1_1, in2_0, in2_1]
          , computationVars = [out_0, out_1, lamda]
          , constraintVars  = [ cz_in, cz_out, cz_inv ] -- Variables internal to IsZero check
          , computations    = [comp0]
          , constraints     = allConstraints
          , pfRecipExpressions = denominators
          , returnVars = []
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assertions

    -- checking that detectBugs returned Right () (no errors)

    let errors = fromLeft [] bugResult
    let errorString = unlines errors
    errorString `shouldSatisfy` ("Denominator expression `Sub (Var \"in2_0\") (Var \"in1_0\")` might be zero" `isInfixOf`)