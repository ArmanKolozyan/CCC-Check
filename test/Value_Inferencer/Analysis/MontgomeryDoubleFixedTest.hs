{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

-- FIXED V-CIRCOMLIB-VUL-005 from Veridise Circomlib audit
module Value_Inferencer.Analysis.MontgomeryDoubleFixedTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- FIXED V-CIRCOMLIB-VUL-005 from Veridise Circomlib audit
spec :: Spec
spec = describe "Fixed MontgomeryDouble template test" $ do
  it "detects no division by zero errors" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- constants from the template
    -- (WE WORKED THE EQUATIONS OUT BECAUSE CIRC NORMALLY DOES THIS AS WELL!)   
    let a = 168_700
    let d_ = 168_696
    let constA = 168_698
    let constB = 1

    -- original variables
    let in_0  = Binding { name = "in_0",  vid = 0, sort = FieldMod p }
    let in_1  = Binding { name = "in_1",  vid = 1, sort = FieldMod p }
    let out_0 = Binding { name = "out_0", vid = 2, sort = FieldMod p }
    let out_1 = Binding { name = "out_1", vid = 3, sort = FieldMod p }
    let lamda = Binding { name = "lamda", vid = 4, sort = FieldMod p }
    let x1_2  = Binding { name = "x1_2",  vid = 5, sort = FieldMod p }

    -- variables for IsZero instance checkZero (for denominator 2*B*in[1])
    let cz_in  = Binding { name = "cz_in",  vid = 10, sort = FieldMod p }
    let cz_out = Binding { name = "cz_out", vid = 11, sort = FieldMod p }
    let cz_inv = Binding { name = "cz_inv", vid = 12, sort = FieldMod p }

    -- computations
    
    let comp0 = Assign "x1_2" (Mul (Var "in_0") (Var "in_0"))
    let numerator = Add (Add (Mul (Int 3) (Var "x1_2"))
                             (Mul (Mul (Int 2) (Int constA)) (Var "in_0")))
                        (Int 1)
    let denominator = Mul (Int 2) (Var "in_1")
    let comp1 = Assign "lamda" (Mul numerator (PfRecip denominator))

    -- constraints

    -- constraints for checkZero instance (checks 2*B*in[1] != 0)
    let cz_assign_in = EqC 100 (Var "cz_in") denominator -- cz_in <= denominator
    let cz_c1 = EqC 101 (Add (Var "cz_out") (Mul (Var "cz_in") (Var "cz_inv"))) (Int 1)
    let cz_c2 = EqC 102 (Mul (Var "cz_in") (Var "cz_out")) (Int 0)
    let cz_force_out = EqC 103 (Var "cz_out") (Int 0) -- cz_out === 0 (enforces cz_in != 0)

    -- original MontgomeryDouble constraints
    let c1 = EqC 200 (Mul (Var "lamda") denominator) numerator
    let c2 = EqC 201 (Var "out_0")
                     (Sub (Sub (Mul (Int constB) (Mul (Var "lamda") (Var "lamda")))
                               (Int constA))
                          (Mul (Int 2) (Var "in_0")))
    let c3 = EqC 202 (Var "out_1")
                     (Sub (Mul (Var "lamda") (Sub (Var "in_0") (Var "out_0")))
                          (Var "in_1"))

    -- combining all constraints
    let allConstraints = [ cz_assign_in, cz_c1, cz_c2, cz_force_out
                         , c1, c2, c3
                         ]

    -- expressions used in PfRecip (denominators)
    let denominators = [ denominator -- from comp1
                       ]

    -- the test program
    let testProgram = Program
          { inputs          = [in_0, in_1]
          , computationVars = [out_0, out_1, lamda, x1_2]
          , constraintVars  = [ cz_in, cz_out, cz_inv ]
          , computations    = [comp0, comp1]
          , constraints     = allConstraints
          , pfRecipExpressions = denominators
          , returnVars = []
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assertions

    -- checking that detectBugs returned Right () (no errors)
    bugResult `shouldSatisfy` isRight
    bugResult `shouldBe` Right ()