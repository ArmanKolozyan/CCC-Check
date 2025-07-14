{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.XorTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

spec :: Spec
spec = describe "XOR Gate Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- inputs
    let a = Binding { name = "a", vid = 0, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    let b = Binding { name = "b", vid = 1, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- output
    let out = Binding { name = "out", vid = 2, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- constraints

    -- XOR gate constraint: out <== a + b - 2*a*b
    let c_xor_def = EqC 100 (Var "out") (Sub (Add (Var "a") (Var "b")) (Mul (Int 2) (Mul (Var "a") (Var "b"))))

    -- all constraints
    let allConstraints = [ c_xor_def ]

    -- the test program
    let testProgram = Program
          { inputs          = [a, b] -- inputs whose tags are enforced
          , computationVars = []
          , constraintVars  = [ out ]
          , computations    = []
          , constraints     = allConstraints
          , pfRecipExpressions = []
          , returnVars = [out] -- expected output
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight