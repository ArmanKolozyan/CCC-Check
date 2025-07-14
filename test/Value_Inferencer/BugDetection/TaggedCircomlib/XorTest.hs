{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.XorTest (spec, xorTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | XOR Gate template test program
xorTestProgram :: Program
xorTestProgram = 
  let
    
    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- inputs
    a = Binding { name = "a", vid = 0, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    b = Binding { name = "b", vid = 1, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- output
    out = Binding { name = "out", vid = 2, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- constraints

    -- XOR gate constraint: out <== a + b - 2*a*b
    c_xor_def = EqC 100 (Var "out") (Sub (Add (Var "a") (Var "b")) (Mul (Int 2) (Mul (Var "a") (Var "b"))))

    -- all constraints
    allConstraints = [ c_xor_def ]

  in Program
      { inputs          = [a, b] -- inputs whose tags are enforced
      , computationVars = []
      , constraintVars  = [ out ]
      , computations    = []
      , constraints     = allConstraints
      , pfRecipExpressions = []
      , returnVars = [out] -- expected output
      }

spec :: Spec
spec = describe "XOR Gate Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do
    -- running the bug detection
    let bugResult = detectBugs xorTestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight