{-# LANGUAGE OverloadedStrings #-}

-- V-CIRCOMLIB-VUL-002 from Veridise Circomlib audit
module Value_Inferencer.BugDetection.Edwards2MontgomeryWrongTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isLeft, fromLeft)
import Data.List (isInfixOf)

-- V-CIRCOMLIB-VUL-002 from Veridise Circomlib audit
spec :: Spec
spec = describe "Edwards2Montgomery template test" $ do
  it "detects potential division by zero" $ do
    
    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- variables
    let in0  = Binding { name = "in0",  vid = 0, sort = FieldMod p }
    let in1  = Binding { name = "in1",  vid = 1, sort = FieldMod p }
    let out0 = Binding { name = "out0", vid = 2, sort = FieldMod p }
    let out1 = Binding { name = "out1", vid = 3, sort = FieldMod p }

    -- computations

    -- out0 <-- (1 + in1) * PfRecip(1 - in1)
    let comp0 = Assign "out0" (Mul (Add (Int 1) (Var "in1")) (PfRecip (Sub (Int 1) (Var "in1"))))
    -- out1 <-- out0 * PfRecip(in0)
    let comp1 = Assign "out1" (Mul (Var "out0") (PfRecip (Var "in0")))

    -- constraints

    -- out0 * (1 - in1) === (1 + in1)
    let constraint1ID = 10
    let c1 = EqC constraint1ID
                  (Mul (Var "out0") (Sub (Int 1) (Var "in1")))
                  (Add (Int 1) (Var "in1"))

    -- out1 * in0 === out0
    let constraint2ID = 11
    let c2 = EqC constraint2ID
                  (Mul (Var "out1") (Var "in0"))
                  (Var "out0")

    -- expressions used in PfRecip (denominators)
    let denominators = [ Sub (Int 1) (Var "in1") -- from comp0
                       , Var "in0"               -- from comp1
                       ]

    -- the test program
    let testProgram = Program
          { inputs          = [in0, in1]
          , computationVars = [out0, out1] 
          , constraintVars  = []           
          , computations    = [comp0, comp1] 
          , constraints     = [c1, c2]       
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

    -- 3. specifically checking for errors related to both denominators
    let errorString = unlines errors
    errorString `shouldSatisfy` ("Denominator expression `Sub (Int 1) (Var \"in1\")` might be zero" `isInfixOf`)
    errorString `shouldSatisfy` ("Denominator expression `Var \"in0\"` might be zero" `isInfixOf`)