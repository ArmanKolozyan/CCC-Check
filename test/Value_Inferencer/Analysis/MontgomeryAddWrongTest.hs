{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

-- V-CIRCOMLIB-VUL-004 from Veridise Circomlib audit
module Value_Inferencer.Analysis.MontgomeryAddWrongTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isLeft, fromLeft)
import Data.List (isInfixOf)

-- V-CIRCOMLIB-VUL-004 from Veridise Circomlib audit
spec :: Spec
spec = describe "MontgomeryAdd template test (Wrong)" $ do
  it "detects potential division by zero" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- constants from the template
    -- (WE WORKED THE EQUATIONS OUT BECAUSE CIRC NORMALLY DOES THIS AS WELL!)
    let a = 168_700
    let d = 168_696 
    let constA = 168_698 -- (2 * (a + d)) `div` (a - d)
    let constB = 1      -- 4 `div` (a - d) 

    -- variables
    let in1_0 = Binding { name = "in1_0", vid = 0, sort = FieldMod p }
    let in1_1 = Binding { name = "in1_1", vid = 1, sort = FieldMod p }
    let in2_0 = Binding { name = "in2_0", vid = 2, sort = FieldMod p }
    let in2_1 = Binding { name = "in2_1", vid = 3, sort = FieldMod p }
    let out_0 = Binding { name = "out_0", vid = 4, sort = FieldMod p }
    let out_1 = Binding { name = "out_1", vid = 5, sort = FieldMod p }
    let lamda = Binding { name = "lamda", vid = 6, sort = FieldMod p }

    -- computations

    -- lamda <-- (in2[1] - in1[1]) / (in2[0] - in1[0])
    let comp0 = Assign "lamda" (Mul (Sub (Var "in2_1") (Var "in1_1"))
                                     (PfRecip (Sub (Var "in2_0") (Var "in1_0"))))

    -- constraints

    -- lamda * (in2[0] - in1[0]) === (in2[1] - in1[1])
    let constraint1ID = 10
    let c1 = EqC constraint1ID
                  (Mul (Var "lamda") (Sub (Var "in2_0") (Var "in1_0")))
                  (Sub (Var "in2_1") (Var "in1_1"))

    -- out[0] <== B*lamda*lamda - A - in1[0] -in2[0]
    -- Representing <== as === for analysis purposes
    let constraint2ID = 11
    let c2 = EqC constraint2ID
                  (Var "out_0")
                  (Sub (Sub (Sub (Mul (Int constB) (Mul (Var "lamda") (Var "lamda")))
                                 (Int constA))
                            (Var "in1_0"))
                       (Var "in2_0"))

    -- out[1] <== lamda * (in1[0] - out[0]) - in1[1]
    let constraint3ID = 12
    let c3 = EqC constraint3ID
                  (Var "out_1")
                  (Sub (Mul (Var "lamda") (Sub (Var "in1_0") (Var "out_0")))
                       (Var "in1_1"))

    -- expressions used in PfRecip (denominators)
    let denominators = [ Sub (Var "in2_0") (Var "in1_0") -- from comp0
                       ]

    -- the test program
    let testProgram = Program
          { inputs          = [in1_0, in1_1, in2_0, in2_1]
          , computationVars = [out_0, out_1, lamda]
          , constraintVars  = []
          , computations    = [comp0]
          , constraints     = [c1, c2, c3]
          , pfRecipExpressions = denominators
          , returnVars = []
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assertions

    -- 1. checking that detectBugs returned an error (Left)
    bugResult `shouldSatisfy` isLeft

    -- 2. checking that the error messages mention potential division by zero
    let errors = fromLeft [] bugResult
    errors `shouldSatisfy` any ("Potential division by zero" `isInfixOf`)

    -- 3. specifically checking for the error related to the denominator
    let errorString = unlines errors
    errorString `shouldSatisfy` ("Denominator expression `Sub (Var \"in2_0\") (Var \"in1_0\")` might be zero" `isInfixOf`)
