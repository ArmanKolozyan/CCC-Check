{-# LANGUAGE OverloadedStrings #-}

-- FIXED V-CIRCOMLIB-VUL-002 from Veridise Circomlib audit
module Value_Inferencer.BugDetection.Veridise.Edwards2MontgomeryFixedTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- FIXED V-CIRCOMLIB-VUL-002 from Veridise Circomlib audit
spec :: Spec
spec = describe "Fixed Edwards2Montgomery template test" $ do
  it "detects no division by zero errors" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- original variables
    let in0  = Binding { name = "in0",  vid = 0, sort = FieldMod p }
    let in1  = Binding { name = "in1",  vid = 1, sort = FieldMod p }
    let out0 = Binding { name = "out0", vid = 2, sort = FieldMod p }
    let out1 = Binding { name = "out1", vid = 3, sort = FieldMod p }

    -- variables for IsZero instance checkZero0
    let cz0_in  = Binding { name = "cz0_in",  vid = 10, sort = FieldMod p }
    let cz0_out = Binding { name = "cz0_out", vid = 11, sort = FieldMod p }
    let cz0_inv = Binding { name = "cz0_inv", vid = 12, sort = FieldMod p }

    -- variables for IsZero instance checkZero1
    let cz1_in  = Binding { name = "cz1_in",  vid = 20, sort = FieldMod p }
    let cz1_out = Binding { name = "cz1_out", vid = 21, sort = FieldMod p }
    let cz1_inv = Binding { name = "cz1_inv", vid = 22, sort = FieldMod p }

    -- computations

    let comp0 = Assign "out0" (Mul (Add (Int 1) (Var "in1")) (PfRecip (Sub (Int 1) (Var "in1"))))
    let comp1 = Assign "out1" (Mul (Var "out0") (PfRecip (Var "in0")))

    -- constraints

    -- constraints for checkZero0 instance
    let cz0_assign_in = EqC 100 (Var "cz0_in") (Var "in0") -- cz0_in <= in0
    let cz0_c1 = EqC 101 (Add (Var "cz0_out") (Mul (Var "cz0_in") (Var "cz0_inv"))) (Int 1)
    let cz0_c2 = EqC 102 (Mul (Var "cz0_in") (Var "cz0_out")) (Int 0)
    let cz0_force_out = EqC 103 (Var "cz0_out") (Int 0) -- cz0_out === 0

    -- constraints for checkZero1 instance
    let cz1_assign_in = EqC 200 (Var "cz1_in") (Sub (Int 1) (Var "in1")) -- cz1_in <= 1 - in1
    let cz1_c1 = EqC 201 (Add (Var "cz1_out") (Mul (Var "cz1_in") (Var "cz1_inv"))) (Int 1)
    let cz1_c2 = EqC 202 (Mul (Var "cz1_in") (Var "cz1_out")) (Int 0)
    let cz1_force_out = EqC 203 (Var "cz1_out") (Int 0) -- cz1_out === 0

    -- original Edwards2Montgomery constraints
    let ed_c1 = EqC 300 (Mul (Var "out0") (Sub (Int 1) (Var "in1"))) (Add (Int 1) (Var "in1"))
    let ed_c2 = EqC 301 (Mul (Var "out1") (Var "in0")) (Var "out0")

    -- combining all constraints
    let allConstraints = [ cz0_assign_in, cz0_c1, cz0_c2, cz0_force_out
                         , cz1_assign_in, cz1_c1, cz1_c2, cz1_force_out
                         , ed_c1, ed_c2
                         ]

    -- expressions used in PfRecip (denominators)
    let denominators = [ Sub (Int 1) (Var "in1") -- from comp0
                       , Var "in0"               -- from comp1
                       ]

    -- the test program
    let testProgram = Program
          { inputs          = [in0, in1]
          , computationVars = [out0, out1]
          , constraintVars  = [ cz0_in, cz0_out, cz0_inv
                              , cz1_in, cz1_out, cz1_inv
                              ]
          , computations    = [comp0, comp1]
          , constraints     = allConstraints
          , pfRecipExpressions = denominators
          , returnVars = []
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assertions

    -- checking that detectBugs returned Right [] (no errors)
    bugResult `shouldSatisfy` isRight
    bugResult `shouldBe` Right ()
  