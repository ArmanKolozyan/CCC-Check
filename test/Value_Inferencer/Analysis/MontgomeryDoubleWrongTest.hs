{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

-- V-CIRCOMLIB-VUL-005 from Veridise Circomlib audit
module Value_Inferencer.Analysis.MontgomeryDoubleWrongTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isLeft, fromLeft)
import Data.List (isInfixOf)

-- V-CIRCOMLIB-VUL-005 from Veridise Circomlib audit
spec :: Spec
spec = describe "MontgomeryDouble template test (Wrong)" $ do
  it "detects potential division by zero" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- constants from the template
    -- (WE WORKED THE EQUATIONS OUT BECAUSE CIRC NORMALLY DOES THIS AS WELL!)
    let a = 168_700
    let d_ = 168_696
    let constA = 168_698 -- (2 * (a + d_)) `div` (a - d_)
    let constB = 1      -- 4 `div` (a - d_)

    -- variables
    let in_0  = Binding { name = "in_0",  vid = 0, sort = FieldMod p }
    let in_1  = Binding { name = "in_1",  vid = 1, sort = FieldMod p }
    let out_0 = Binding { name = "out_0", vid = 2, sort = FieldMod p }
    let out_1 = Binding { name = "out_1", vid = 3, sort = FieldMod p }
    let lamda = Binding { name = "lamda", vid = 4, sort = FieldMod p }
    let x1_2  = Binding { name = "x1_2",  vid = 5, sort = FieldMod p }

    -- computations

    -- x1_2 <== in[0] * in[0]
    let comp0 = Assign "x1_2" (Mul (Var "in_0") (Var "in_0"))

    -- lamda <-- (3*x1_2 + 2*A*in[0] + 1 ) / (2*B*in[1])
    let numerator = Add (Add (Mul (Int 3) (Var "x1_2"))
                             (Mul (Mul (Int 2) (Int constA)) (Var "in_0")))
                        (Int 1)
    let denominator = Mul (Int 2) (Var "in_1")
    let comp1 = Assign "lamda" (Mul numerator (PfRecip denominator))

    -- constraints

    -- lamda * (2*B*in[1]) === (3*x1_2 + 2*A*in[0] + 1)
    let constraint1ID = 10
    let c1 = EqC constraint1ID (Mul (Var "lamda") denominator) numerator

    -- out[0] <== B*lamda*lamda - A - 2*in[0]
    let constraint2ID = 11
    let c2 = EqC constraint2ID
                  (Var "out_0")
                  (Sub (Sub (Mul (Int constB) (Mul (Var "lamda") (Var "lamda")))
                            (Int constA))
                       (Mul (Int 2) (Var "in_0")))

    -- out[1] <== lamda * (in[0] - out[0]) - in[1]
    let constraint3ID = 12
    let c3 = EqC constraint3ID
                  (Var "out_1")
                  (Sub (Mul (Var "lamda") (Sub (Var "in_0") (Var "out_0")))
                       (Var "in_1"))

    -- expressions used in PfRecip (denominators)
    let denominators = [ denominator -- from comp1
                       ]

    -- the test program
    let testProgram = Program
          { inputs          = [in_0, in_1]
          , computationVars = [out_0, out_1, lamda, x1_2]
          , constraintVars  = []
          , computations    = [comp0, comp1]
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
    errorString `shouldSatisfy` ("Denominator expression `Mul (Int 2) (Var \"in_1\")` might be zero" `isInfixOf`)
