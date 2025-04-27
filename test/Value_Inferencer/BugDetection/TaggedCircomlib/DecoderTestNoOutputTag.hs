{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.DecoderTestNoOutputTag (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

spec :: Spec
spec = describe "Decoder(2) V3 Template Test" $ do
  it "should complete analysis without detecting bugs after removing `success` tag" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- inputs
    let inp = Binding { name = "inp", vid = 0, sort = FieldMod p, tag = Nothing }

    -- outputs
    let out = Binding { name = "out", vid = 1, sort = ArraySort (FieldMod p) 2, tag = Just (SimpleTag "binary") }
    let success = Binding { name = "success", vid = 2, sort = FieldMod p, tag = Nothing } -- tag removed

    -- intermediate signals from IsZero instances (flattened)
    -- instance 0 (for i=0)
    let cz0_in = Binding { name = "cz0_in", vid = 3, sort = FieldMod p, tag = Nothing }
    let cz0_inv = Binding { name = "cz0_inv", vid = 4, sort = FieldMod p, tag = Nothing } -- 'inv' in IsZero has no tag
    let cz0_out = Binding { name = "cz0_out", vid = 5, sort = FieldMod p, tag = Just (SimpleTag "binary") } -- 'out' in IsZero is binary

    -- instance 1 (for i=1)
    let cz1_in = Binding { name = "cz1_in", vid = 6, sort = FieldMod p, tag = Nothing }
    let cz1_inv = Binding { name = "cz1_inv", vid = 7, sort = FieldMod p, tag = Nothing }
    let cz1_out = Binding { name = "cz1_out", vid = 8, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- constraints

    -- connections to IsZero inputs
    let c_cz0_in = EqC 100 (Var "cz0_in") (Var "inp") -- cz0_in <== inp - 0
    let c_cz1_in = EqC 101 (Var "cz1_in") (Sub (Var "inp") (Int 1)) -- cz1_in <== inp - 1

    -- IsZero[0] constraints
    let c_cz0_out_def = EqC 102 (Var "cz0_out") (Add (Mul (Int (-1)) (Mul (Var "cz0_in") (Var "cz0_inv"))) (Int 1)) -- cz0_out <== -cz0_in*cz0_inv +1
    let c_cz0_zero_chk = EqC 103 (Mul (Var "cz0_in") (Var "cz0_out")) (Int 0) -- cz0_in*cz0_out === 0

    -- IsZero[1] constraints
    let c_cz1_out_def = EqC 104 (Var "cz1_out") (Add (Mul (Int (-1)) (Mul (Var "cz1_in") (Var "cz1_inv"))) (Int 1)) -- cz1_out <== -cz1_in*cz1_inv +1
    let c_cz1_zero_chk = EqC 105 (Mul (Var "cz1_in") (Var "cz1_out")) (Int 0) -- cz1_in*cz1_out === 0

    -- constraint for success output (success === sum of czX_out)
    let c_success = EqC 106 (Var "success") (Add (Var "cz0_out") (Var "cz1_out")) -- success <== lc; lc = cz0_out + cz1_out

    -- constraint defining the 'out' array structure based on IsZero outputs
    -- out === array(cz0_out, cz1_out)
    let c_out_def = EqC 107 (Var "out") (ArrayConstruct [Var "cz0_out", Var "cz1_out"] (ArraySort (FieldMod p) 2))

    -- Explicit binary constraints for IsZero outputs (added artificially).
    -- These are added because the analysis cannot derive czX_out is binary from IsZero logic alone.
    let c_cz0_binary_chk = EqC 108 (Mul (Var "cz0_out") (Sub (Var "cz0_out") (Int 1))) (Int 0) -- cz0_out * (cz0_out - 1) === 0
    let c_cz1_binary_chk = EqC 109 (Mul (Var "cz1_out") (Sub (Var "cz1_out") (Int 1))) (Int 0) -- cz1_out * (cz1_out - 1) === 0

    -- all constraints
    let allConstraints = [ c_cz0_in, c_cz1_in
                         , c_cz0_out_def, c_cz0_zero_chk, c_cz0_binary_chk
                         , c_cz1_out_def, c_cz1_zero_chk, c_cz1_binary_chk
                         , c_success
                         , c_out_def
                         ]

    -- the test program
    let testProgram = Program
          { inputs          = [inp]
          , computationVars = []
          , constraintVars  = [out, success, cz0_in, cz0_inv, cz0_out, cz1_in, cz1_inv, cz1_out]
          , computations    = []
          , constraints     = allConstraints
          , pfRecipExpressions = []
          , returnVars = [out, success]
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- 1. cz0_out/cz1_out: Passed tag check due to explicit binary constraints.
    -- 2. out[0]/out[1]: Passed tag check because array elements are binary.
    -- 3. success: Inferred domain is {0,1,2}. Since the 'binary' tag was removed from its binding,
    --    this domain is now accepted, and no error is reported.

    bugResult `shouldSatisfy` isRight
