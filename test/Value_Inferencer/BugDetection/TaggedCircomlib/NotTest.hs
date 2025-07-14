{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.NotTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

spec :: Spec
spec = describe "NOT Gate Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- input
    let inp = Binding { name = "in", vid = 0, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- output
    let out = Binding { name = "out", vid = 1, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- constraints

    -- NOT gate constraint: out <== 1 + in - 2*in
    let c_not_def = EqC 100 (Var "out") (Sub (Add (Int 1) (Var "in")) (Mul (Int 2) (Var "in")))

    -- all constraints
    let allConstraints = [ c_not_def ]

    -- the test program
    let testProgram = Program
          { inputs          = [inp] -- input whose tag is enforced
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