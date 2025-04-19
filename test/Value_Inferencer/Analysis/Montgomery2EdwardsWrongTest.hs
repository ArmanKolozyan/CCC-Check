{-# LANGUAGE OverloadedStrings #-}

-- V-CIRCOMLIB-VUL-003 from Veridise Circomlib audit
module Value_Inferencer.Analysis.Montgomery2EdwardsWrongTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isLeft, fromLeft)
import Data.List (isInfixOf)

-- V-CIRCOMLIB-VUL-003 from Veridise Circomlib audit
spec :: Spec
spec = describe "Montgomery2Edwards template test (Wrong)" $ do
  it "detects potential division by zero" $ do
    
    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- variables
    let in0  = Binding { name = "in0",  vid = 0, sort = FieldMod p }
    let in1  = Binding { name = "in1",  vid = 1, sort = FieldMod p }
    let out0 = Binding { name = "out0", vid = 2, sort = FieldMod p }
    let out1 = Binding { name = "out1", vid = 3, sort = FieldMod p }

    -- computations

    -- out0 <-- in0 / in1
    let comp0 = Assign "out0" (Mul (Var "in0") (PfRecip (Var "in1")))
    -- out1 <-- (in0 - 1) / (in0 + 1)
    let comp1 = Assign "out1" (Mul (Sub (Var "in0") (Int 1)) (PfRecip (Add (Var "in0") (Int 1))))

    -- constraints

    -- out0 * in1 === in0
    let constraint1ID = 10
    let c1 = EqC constraint1ID
                  (Mul (Var "out0") (Var "in1"))
                  (Var "in0")

    -- out1 * (in0 + 1) === in0 - 1
    let constraint2ID = 11
    let c2 = EqC constraint2ID
                  (Mul (Var "out1") (Add (Var "in0") (Int 1)))
                  (Sub (Var "in0") (Int 1))

    -- expressions used in PfRecip (denominators)
    let denominators = [ Var "in1"               -- from comp0
                       , Add (Var "in0") (Int 1) -- from comp1
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
    errorString `shouldSatisfy` ("Denominator expression `Var \"in1\"` might be zero" `isInfixOf`)
    errorString `shouldSatisfy` ("Denominator expression `Add (Var \"in0\") (Int 1)` might be zero" `isInfixOf`)