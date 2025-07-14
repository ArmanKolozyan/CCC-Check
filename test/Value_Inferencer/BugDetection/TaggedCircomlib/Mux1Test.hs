{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.Mux1Test (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

spec :: Spec
spec = describe "Mux1 Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- inputs (selector and constants)
    let s = Binding { name = "s", vid = 0, sort = FieldMod p, tag = Just $ SimpleTag "binary" }
    let c_0 = Binding { name = "c_0", vid = 1, sort = FieldMod p, tag = Nothing } -- c[0]
    let c_1 = Binding { name = "c_1", vid = 2, sort = FieldMod p, tag = Nothing } -- c[1]

    -- MultiMux1(1) component internal signal (intermediate output from the component)
    let mux_out_0 = Binding { name = "mux_out_0", vid = 3, sort = FieldMod p, tag = Nothing } -- mux.out[0]

    -- final output
    let out = Binding { name = "out", vid = 4, sort = FieldMod p, tag = Nothing }

    -- constraints

    -- MultiMux1(1) component logic:
    -- mux.out[0] <== (c[1] - c[0]) * s + c[0]
    let c_multimux = EqC 100 (Var "mux_out_0") 
          (Add (Mul (Sub (Var "c_1") (Var "c_0")) (Var "s")) (Var "c_0"))

    -- output assignment: out <== mux.out[0]
    let c_output = EqC 101 (Var "out") (Var "mux_out_0")

    -- all constraints
    let allConstraints = [ c_multimux, c_output ]

    -- the test program
    let testProgram = Program
          { inputs          = [s, c_0, c_1] -- selector and constant inputs
          , computationVars = []
          , constraintVars  = [ mux_out_0, out ]
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