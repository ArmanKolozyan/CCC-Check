{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.NotTest (spec, notTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | NOT Gate template test program
notTestProgram :: Program
notTestProgram = 
  let
    
    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- input
    inp = Binding { name = "in", vid = 0, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- output
    out = Binding { name = "out", vid = 1, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- constraints

    -- NOT gate constraint: out <== 1 + in - 2*in
    c_not_def = EqC 100 (Var "out") (Sub (Add (Int 1) (Var "in")) (Mul (Int 2) (Var "in")))

    -- all constraints
    allConstraints = [ c_not_def ]

  in Program
      { inputs          = [inp] -- input whose tag is enforced
      , computationVars = []
      , constraintVars  = [ out ]
      , computations    = []
      , constraints     = allConstraints
      , pfRecipExpressions = []
      , returnVars = [out] -- expected output
      }

spec :: Spec
spec = describe "NOT Gate Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do
    -- running the bug detection
    let bugResult = detectBugs notTestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight