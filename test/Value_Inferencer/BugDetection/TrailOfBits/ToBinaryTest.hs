{-# LANGUAGE OverloadedStrings #-}

-- Test for ToBinary template demonstrating analysis limitations
module Value_Inferencer.BugDetection.TrailOfBits.ToBinaryTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import qualified Data.Map as Map
import Data.Either (isLeft, fromLeft)
import Data.List (isInfixOf, any)

-- Test case for the ToBinary template from https://blog.trailofbits.com/2024/01/02/tag-youre-it-signal-tagging-in-circom/
spec :: Spec
spec = describe "ToBinary template analysis test" $ do
  it "does not deduce that 'out' is binary due to non-relational analysis" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- variables
    let var_in  = Binding { name = "in",  vid = 0, sort = FieldMod p }
    let var_out = Binding { name = "out", vid = 1, sort = Bool }
    let var_inv = Binding { name = "inv", vid = 2, sort = FieldMod p }

    -- computations

    -- inv <-- in!=0 ? 1/in : 0
    let comp_inv = Assign "inv" (Ite (Not (Eq (Var "in") (Int 0)))
                                (PfRecip (Var "in"))    
                                (Int 0))   

    -- out <== in*inv
    let comp_out = Assign "out" (Mul (Var "in") (Var "inv"))                              

    -- constraints

    -- out <== in*inv 
    let constraint1ID = 10
    let c1 = EqC constraint1ID
                  (Var "out")
                  (Mul (Var "in") (Var "inv"))

    -- in*(1 - out) === 0
    let constraint2ID = 11
    let c2 = EqC constraint2ID
                  (Mul (Var "in") (Sub (Int 1) (Var "out")))
                  (Int 0)

    -- the test program
    let testProgram = Program
          { inputs          = [var_in]
          , computationVars = [var_inv]
          , constraintVars  = [var_out, var_inv]
          , computations    = [comp_inv, comp_out]
          , constraints     = [c1, c2]
          , pfRecipExpressions = []
          , returnVars = []
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assertions

    -- 1. Checking that detectBugs returned an error (Left)
    bugResult `shouldSatisfy` isLeft

    -- 2. Extracting the errors
    let errors = fromLeft [] bugResult

    -- 3. Checking that the errors contain the expected message about 'out'
    let expectedErrorSubstring = "Boolean variable `out` has upper bound > 1"
    errors `shouldSatisfy` any (expectedErrorSubstring `isInfixOf`)

    -- Explanation:
    -- The constraint `in*(1 - out) === 0` mathematically implies that either `in == 0` or `out == 1`.
    -- The constraint `out === in*inv` implies that if `in == 0`, then `out == 0`.
    -- Together, these correctly force `out` to be binary (0 or 1).
    --
    -- However, the value analysis used by `detectBugs` is non-relational. It tracks the possible values for each variable independently.
    -- 1. The analysis cannot deduce the implication "either `in` is 0 OR `1 - out` is 0" from `in*(1 - out) === 0`.
    -- 2. Consequently, the analysis fails to refine the domain of `out` from its initial default (e.g., `[0, p-1]`) to `{0, 1}`.
    -- 3. Since `out` is tagged with `sort = Bool`, `detectBugs` performs a check comparing the inferred domain (`[0, p-1]`)
    --    against the expected domain for `Bool` (`{0, 1}`).
    -- 4. Because the inferred domain is larger than `{0, 1}`, `detectBugs` reports a sort violation error for `out`.

    -- To correctly infer that out is binary, the analysis must support relational reasoning between variables. This would allow it to deduce that in*(1 - out) === 0 
    -- implies either in == 0 or out == 1, and that out === in*inv ensures out == 0 when in == 0. 
    -- Current non-relational analysis tracks variables independently and cannot capture these interdependencies. 
    -- A more advanced abstract domain or path-sensitive analysis is thus required.
