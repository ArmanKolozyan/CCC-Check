{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.IsZeroTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (fromLeft)
import Data.List (isInfixOf)

spec :: Spec
spec = describe "IsZero Template Test" $ do
  it "detects expected analysis limitations for IsZero template" $ do

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

    -- all constraints
    let allConstraints = [ c_iszero_def, c_correctness ]

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

    let errors = fromLeft [] bugResult
    let errorString = unlines errors

    -- The analysis cannot deduce that 'out' is binary ({0,1}) from the IsZero constraints
    -- (out = -in*inv + 1, in*out = 0), as the analysis lacks symbolic reasoning to perform 
    -- case analysis (if in=0 vs in!=0). Moreover, the ValueDomain is non-relational; it 
    -- cannot combine the implications of out = 1 - in*inv and in*out = 0 symbolically to 
    -- deduce out must be 0 or 1. The domain of 'out' remains bounded by the field modulus 'p'.
    -- Therefore, 'out' fails its 'binary' tag check since its inferred domain is not {0,1}.

    -- assertions
    errorString `shouldSatisfy` ("Boolean variable `out`" `isInfixOf`)