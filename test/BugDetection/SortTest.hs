{-# LANGUAGE OverloadedStrings #-}

module BugDetection.SortTest (spec) where

import Syntax.AST
import Test.Hspec
import BugDetection.BugDetection (detectBugs)

spec :: Spec
spec = describe "Bug Detection Tests" $ do
  -- 1) A scenario in which each variable is properly constrained.
  it "No-Bug scenario: detectBugs returns Right ()" $ do
    -- We define a small program with:
    --   - var b: declared Bool, forced to {0,1}
    --   - var nz: appears as denominator, forced to {1,2}
    --   - var f: declared FieldMod 4, forced to [0..3]

    -- bindings:
    let b = Binding {name = "b", vid = 0, sort = Bool}
    let nz = Binding {name = "nz", vid = 1, sort = FieldMod 5}
    let f = Binding {name = "f", vid = 2, sort = FieldMod 4}

    -- nz as denominator
    let cNZ_denom =
          EqC
            99
            (Mul (Int 1) (PfRecip (Var "nz")))
            (Int 1)

    -- constraints:
    --  for b:   b*(b-1) = 0  forces b in {0,1}
    let cB_binary =
          EqC
            100
            (Mul (Var "b") (Sub (Var "b") (Int 1)))
            (Int 0)

    --  for nz: (nz-1)*(nz-2)=0 => possible {1,2}. 
    let cNZ_poly =
          EqC
            101
            (Mul (Sub (Var "nz") (Int 1)) (Sub (Var "nz") (Int 2)))
            (Int 0)

    -- for f: (b0 + 2*b1) = f

    -- intermediate variables b0 and b1
    let b0 = Binding { name = "b0", vid = 3, sort = Bool }
    let b1 = Binding { name = "b1", vid = 4, sort = Bool }

    -- constraints:

    -- (b0 * (b0 - 1)) = 0
    let b0_eq = EqC 102 (Mul (Var "b0") (Sub (Var "b0") (Int 1))) (Int 0)

    -- (b1 * (b1 - 1)) = 0
    let b1_eq = EqC 103 (Mul (Var "b1") (Sub (Var "b1") (Int 1))) (Int 0)

    -- (b0 + 2*b1) = f
    let sum_eq = EqC 104 (Add (Var "b0") (Mul (Int 2) (Var "b1"))) (Var "f")

    let constraints = [cNZ_denom, cB_binary, cNZ_poly, b0_eq, b1_eq, sum_eq]

    let programOk =
          Program
            { inputs = [b, nz, f],
              computationVars = [],
              constraintVars = [b0, b1],
              computations = [],
              constraints = constraints,
              returnVars = [],
              pfRecipExpressions = [Var "nz"]
            }

    case detectBugs programOk Nothing of
      Left errs -> expectationFailure $ "Should have no bugs, but got errors: " ++ show errs
      Right () -> pure ()

  ----------------------------------------------------------------------------

  -- 2) A scenario in which not all variables are properly constrained.
  it "Bug scenario: omit constraints => bug is detected" $ do
    -- we declare the same variables, but omit crucial constraints:
    -- For b: we skip the binary constraint => b can end up being anything => fails the Bool check.
    -- For nz: we skip the polynomial => so it can be 0 => fails NonZero check.
    -- For f: we make it equal to a number > 3.

    let b = Binding {name = "b", vid = 0, sort = Bool}
    let nz = Binding {name = "nz", vid = 1, sort = FieldMod 5}
    let f = Binding {name = "f", vid = 2, sort = FieldMod 5}

    let b_eq = EqC 105 (Var "b") (Int 2)
    let nz_eq = EqC 106 (Var "nz") (Int 0)
    let f_eq = EqC 107 (Var "f") (Int 9)

    -- nz as denominator
    let cNZ_denom =
          EqC
            108
            (Mul (Int 1) (PfRecip (Var "nz")))
            (Int 1)

    let constraints = [b_eq, nz_eq, f_eq, cNZ_denom]

    let programBug =
          Program
            { inputs = [b, nz, f],
              computationVars = [],
              constraintVars = [],
              computations = [],
              constraints = constraints,
              returnVars = [],
              pfRecipExpressions = [Var "nz"]
            }

    case detectBugs programBug Nothing of
      Left errs -> do
        -- we expect bug messages for all variables
        errs `shouldMatchList` -- `shouldMatchList` is chosen over `shouldBe`, as it does not take order into account
          ["Boolean variable `b` has values outside {0,1}: [2]",
          "Potential division by zero: Denominator expression `Var \"nz\"` might be zero.",
          "Variable `f` has out-of-range values: [9] (expected [0..4])"]
      Right () ->
        expectationFailure "We expected bug errors, but detectBugs returned Right ()"
