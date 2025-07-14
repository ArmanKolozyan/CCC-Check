{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.IsZeroForcedTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

spec :: Spec
spec = describe "IsZero Template Test" $ do
  it "successfully completes analysis without detecting bugs when output is forced" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- input (field value)
    let inp = Binding { name = "in", vid = 0, sort = FieldMod p, tag = Nothing }

    -- intermediate signal for inverse calculation
    let inv = Binding { name = "inv", vid = 1, sort = FieldMod p, tag = Nothing }

    -- output (binary)
    let out = Binding { name = "out", vid = 2, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- constraints

    -- out <== -in*inv +1
    let c_iszero_def = EqC 100 (Var "out") (Add (Mul (Int (-1)) (Mul (Var "in") (Var "inv"))) (Int 1))

    -- in*out === 0
    let c_correctness = EqC 101 (Mul (Var "in") (Var "out")) (Int 0)

    -- forcing out to be 1
    let c_force_out = EqC 102 (Var "out") (Int 1)

    -- all constraints
    let allConstraints = [ c_iszero_def, c_correctness, c_force_out ]

    -- the test program
    let testProgram = Program
          { inputs          = [inp] -- input field value
          , computationVars = []
          , constraintVars  = [ inv, out ]
          , computations    = []
          , constraints     = allConstraints
          , pfRecipExpressions = []
          , returnVars = [out] -- expected binary output
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight