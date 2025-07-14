{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.IsEqualTest (spec, isEqualTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (fromLeft)
import Data.List (isInfixOf)

-- | IsEqual template test program (expected to fail binary tag checks)
isEqualTestProgram :: Program
isEqualTestProgram =
  let
    
    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- inputs (array of 2 field values)
    in0 = Binding { name = "in0", vid = 0, sort = FieldMod p, tag = Nothing }
    in1 = Binding { name = "in1", vid = 1, sort = FieldMod p, tag = Nothing }

    -- intermediate signals for IsZero component
    diff = Binding { name = "diff", vid = 2, sort = FieldMod p, tag = Nothing }
    inv = Binding { name = "inv", vid = 3, sort = FieldMod p, tag = Nothing }
    isz_out = Binding { name = "isz_out", vid = 4, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- output (binary)
    out = Binding { name = "out", vid = 5, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- constraints

    -- diff <== in[1] - in[0]
    c_diff = EqC 100 (Var "diff") (Sub (Var "in1") (Var "in0"))

    -- IsZero component logic: isz_out <== -diff*inv + 1
    c_iszero_def = EqC 101 (Var "isz_out") (Add (Mul (Int (-1)) (Mul (Var "diff") (Var "inv"))) (Int 1))

    -- IsZero constraint: diff*isz_out === 0
    c_iszero_correctness = EqC 102 (Mul (Var "diff") (Var "isz_out")) (Int 0)

    -- Output assignment: out <== isz_out
    c_output = EqC 103 (Var "out") (Var "isz_out")

    -- all constraints
    allConstraints = [ c_diff, c_iszero_def, c_iszero_correctness, c_output ]

  in Program
       { inputs          = [in0, in1] -- two field value inputs
       , computationVars = []
       , constraintVars  = [ diff, inv, isz_out, out ]
       , computations    = []
       , constraints     = allConstraints
       , pfRecipExpressions = []
       , returnVars = [out] -- expected binary output
       }

spec :: Spec
spec = describe "IsEqual Template Test" $ do
  it "detects expected analysis limitations for IsEqual template" $ do

    -- running the bug detection
    let bugResult = detectBugs isEqualTestProgram Nothing

    let errors = fromLeft [] bugResult
    let errorString = unlines errors

    -- The analysis cannot deduce that 'isz_out' and 'out' are binary ({0,1}) from the IsEqual 
    -- template constraints. The IsEqual template internally uses an IsZero component with constraints:
    -- (isz_out = -diff*inv + 1, diff*isz_out = 0), where diff = in1 - in0.
    -- The analysis lacks symbolic reasoning to perform case analysis (if diff=0 vs diff≠0).
    -- Moreover, the ValueDomain is non-relational; it cannot combine the implications of 
    -- isz_out = 1 - diff*inv and diff*isz_out = 0 symbolically to deduce isz_out must be 0 or 1.
    -- The domains of 'isz_out' and 'out' remain bounded by the field modulus 'p'.
    -- Therefore, both variables fail their 'binary' tag checks since their inferred domains are not {0,1}.

    -- assertions
    errorString `shouldSatisfy` ("Boolean variable `isz_out`" `isInfixOf`)
    errorString `shouldSatisfy` ("Boolean variable `out`" `isInfixOf`)