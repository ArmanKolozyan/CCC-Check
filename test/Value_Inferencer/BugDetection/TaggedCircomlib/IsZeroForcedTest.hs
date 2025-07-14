{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.IsZeroForcedTest (spec, isZeroForcedTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | IsZero template test program with forced output
isZeroForcedTestProgram :: Program
isZeroForcedTestProgram =
  let

    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- input (field value)
    inp = Binding { name = "in", vid = 0, sort = FieldMod p, tag = Nothing }

    -- intermediate signal for inverse calculation
    inv = Binding { name = "inv", vid = 1, sort = FieldMod p, tag = Nothing }

    -- output (binary)
    out = Binding { name = "out", vid = 2, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- constraints

    -- out <== -in*inv +1
    c_iszero_def = EqC 100 (Var "out") (Add (Mul (Int (-1)) (Mul (Var "in") (Var "inv"))) (Int 1))

    -- in*out === 0
    c_correctness = EqC 101 (Mul (Var "in") (Var "out")) (Int 0)

    -- forcing out to be 1
    c_force_out = EqC 102 (Var "out") (Int 1)

    -- all constraints
    allConstraints = [ c_iszero_def, c_correctness, c_force_out ]
    
  in Program
       { inputs          = [inp] -- input field value
       , computationVars = []
       , constraintVars  = [ inv, out ]
       , computations    = []
       , constraints     = allConstraints
       , pfRecipExpressions = []
       , returnVars = [out] -- expected binary output
       }

spec :: Spec
spec = describe "IsZero Template Test" $ do
  it "successfully completes analysis without detecting bugs when output is forced" $ do

    -- running the bug detection
    let bugResult = detectBugs isZeroForcedTestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight
