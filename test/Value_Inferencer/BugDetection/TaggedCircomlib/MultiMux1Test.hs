{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.MultiMux1Test (spec, multiMux1TestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | MultiMux1 template test program
multiMux1TestProgram :: Program
multiMux1TestProgram =
  let
    
    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- template parameter
    n = 2 -- using 2 elements for this test

    -- bindings

    -- inputs (selector and constants)
    s = Binding { name = "s", vid = 0, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    
    -- Constants array c[n][2] - flattened as c[i][j] becomes c_i_j
    c_0_0 = Binding { name = "c_0_0", vid = 1, sort = FieldMod p, tag = Nothing } -- c[0][0]
    c_0_1 = Binding { name = "c_0_1", vid = 2, sort = FieldMod p, tag = Nothing } -- c[0][1]
    c_1_0 = Binding { name = "c_1_0", vid = 3, sort = FieldMod p, tag = Nothing } -- c[1][0]
    c_1_1 = Binding { name = "c_1_1", vid = 4, sort = FieldMod p, tag = Nothing } -- c[1][1]

    -- outputs
    out_0 = Binding { name = "out_0", vid = 5, sort = FieldMod p, tag = Nothing } -- out[0]
    out_1 = Binding { name = "out_1", vid = 6, sort = FieldMod p, tag = Nothing } -- out[1]

    -- constraints

    -- MultiMux1 logic for each element i:
    -- out[i] <== (c[i][1] - c[i][0]) * s + c[i][0]
    
    -- for i = 0: out[0] <== (c[0][1] - c[0][0]) * s + c[0][0]
    c_mux_0 = EqC 100 (Var "out_0") 
          (Add (Mul (Sub (Var "c_0_1") (Var "c_0_0")) (Var "s")) (Var "c_0_0"))

    -- for i = 1: out[1] <== (c[1][1] - c[1][0]) * s + c[1][0]
    c_mux_1 = EqC 101 (Var "out_1") 
          (Add (Mul (Sub (Var "c_1_1") (Var "c_1_0")) (Var "s")) (Var "c_1_0"))

    -- all constraints
    allConstraints = [ c_mux_0, c_mux_1 ]

  in Program
       { inputs          = [s, c_0_0, c_0_1, c_1_0, c_1_1] -- selector and constant inputs
       , computationVars = []
       , constraintVars  = [ out_0, out_1 ]
       , computations    = []
       , constraints     = allConstraints
       , pfRecipExpressions = []
       , returnVars = [out_0, out_1] -- expected outputs
       }

spec :: Spec
spec = describe "MultiMux1 Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do

    -- running the bug detection
    let bugResult = detectBugs multiMux1TestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight