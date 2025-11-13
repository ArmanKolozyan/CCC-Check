{-# LANGUAGE OverloadedStrings #-}

module ValueInference.BugDetection.TaggedCircomlib.Mux1Test (spec, mux1TestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | Mux1 template test program
mux1TestProgram :: Program
mux1TestProgram =
  let

    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- inputs (selector and constants)
    s = Binding { name = "s", vid = 0, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    c_0 = Binding { name = "c_0", vid = 1, sort = FieldMod p, tag = Nothing } -- c[0]
    c_1 = Binding { name = "c_1", vid = 2, sort = FieldMod p, tag = Nothing } -- c[1]

    -- MultiMux1(1) component internal signal (intermediate output from the component)
    mux_out_0 = Binding { name = "mux_out_0", vid = 3, sort = FieldMod p, tag = Nothing } -- mux.out[0]

    -- final output
    out = Binding { name = "out", vid = 4, sort = FieldMod p, tag = Nothing }

    -- constraints

    -- MultiMux1(1) component logic:
    -- mux.out[0] <== (c[1] - c[0]) * s + c[0]
    c_multimux = EqC 100 (Var "mux_out_0") 
          (Add (Mul (Sub (Var "c_1") (Var "c_0")) (Var "s")) (Var "c_0"))

    -- output assignment: out <== mux.out[0]
    c_output = EqC 101 (Var "out") (Var "mux_out_0")

    -- all constraints
    allConstraints = [ c_multimux, c_output ]
    
  in Program
       { inputs          = [s, c_0, c_1] -- selector and constant inputs
       , computationVars = []
       , constraintVars  = [ mux_out_0, out ]
       , computations    = []
       , constraints     = allConstraints
       , pfRecipExpressions = []
       , returnVars = [out] -- expected output
       }

spec :: Spec
spec = describe "Mux1 Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do

    -- running the bug detection
    let bugResult = detectBugs mux1TestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight