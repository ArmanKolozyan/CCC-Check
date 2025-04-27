{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.ValueAnalysis.LessThanTest (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax.AST
import Test.Hspec
import ValueAnalysis.Analysis

-- Value inferencing a fully flattened (i.e., everything in a single template) 
-- version of the LessThan template. We correctly constrain
-- the input variables to be representable in 2 bits.
spec :: Spec
spec = describe "Value Inferencer Detects" $ do
  it "correct values when using LessThan" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- 1) the bindings

    -- inputs a and b (up to 2 bits => FieldMod 3)
    let a = Binding {name = "a", vid = 0, sort = FieldMod p, tag = Just $ MaxValTag 3}
    let b = Binding {name = "b", vid = 1, sort = FieldMod p, tag = Just $ MaxValTag 3}
    -- bits for a
    let a_b0 = Binding {name = "a_b0", vid = 2, sort = Bool, tag = Just $ SimpleTag "binary"}
    let a_b1 = Binding {name = "a_b1", vid = 3, sort = Bool, tag = Just $ SimpleTag "binary"}
    -- bits for b
    let b_b0 = Binding {name = "b_b0", vid = 4, sort = Bool, tag = Just $ SimpleTag "binary"}
    let b_b1 = Binding {name = "b_b1", vid = 5, sort = Bool, tag = Just $ SimpleTag "binary"}
    -- diff variable inside the LessThan template (up to 3 bits => FieldMod 7)
    let diff = Binding {name = "diff", vid = 6, sort = FieldMod p, tag = Just $ MaxValTag 6}
    -- bits from diff (Num2Bits(3)):
    let diff_b0 = Binding {name = "diff_b0", vid = 7, sort = Bool, tag = Just $ SimpleTag "binary"}
    let diff_b1 = Binding {name = "diff_b1", vid = 8, sort = Bool, tag = Just $ SimpleTag "binary"}
    let diff_b2 = Binding {name = "diff_b2", vid = 9, sort = Bool, tag = Just $ SimpleTag "binary"}
    -- the final output:
    let out = Binding {name = "out", vid = 10, sort = Bool, tag = Just $ SimpleTag "binary"}

    -- 2) the constraints

    -- (a_b0 * (a_b0 - 1)) === 0
    let cA_b0_binary =
          EqC
            100
            ( Mul
                (Var "a_b0")
                (Sub (Var "a_b0") (Int 1))
            )
            (Int 0)
    -- (a_b1 * (a_b1 - 1)) === 0
    let cA_b1_binary =
          EqC
            101
            ( Mul
                (Var "a_b1")
                (Sub (Var "a_b1") (Int 1))
            )
            (Int 0)
    -- (a_b0 + 2*a_b1) === a
    let cReconstructA =
          EqC
            102
            ( Add
                (Var "a_b0")
                (Mul (Int 2) (Var "a_b1"))
            )
            (Var "a")
    -- (b_b1 * (b_b1 - 1)) === 0
    let cB_b0_binary =
          EqC
            103
            ( Mul
                (Var "b_b0")
                (Sub (Var "b_b0") (Int 1))
            )
            (Int 0)
    -- (b_b0 + 2*b_b1) === a
    let cB_b1_binary =
          EqC
            104
            ( Mul
                (Var "b_b1")
                (Sub (Var "b_b1") (Int 1))
            )
            (Int 0)
    -- (b_b0 + 2*b_b1) === b
    let cReconstructB =
          EqC
            105
            ( Add
                (Var "b_b0")
                (Mul (Int 2) (Var "b_b1"))
            )
            (Var "b")
    -- diff === a + 4 - b
    let cComputediff =
          EqC
            106
            (Var "diff")
            ( Sub
                ( Add
                    (Var "a")
                    (Int 4)
                )
                (Var "b")
            )
    -- (diff_b0 * (diff_b0 - 1)) === 0
    let cdiff_b0_binary =
          EqC
            107
            ( Mul
                (Var "diff_b0")
                (Sub (Var "diff_b0") (Int 1))
            )
            (Int 0)
    -- (diff_b1 * (diff_b1 - 1)) === 0
    let cdiff_b1_binary =
          EqC
            108
            ( Mul
                (Var "diff_b1")
                (Sub (Var "diff_b1") (Int 1))
            )
            (Int 0)
    -- (diff_b2 * (diff_b2 - 1)) === 0
    let cdiff_b2_binary =
          EqC
            109
            ( Mul
                (Var "diff_b2")
                (Sub (Var "diff_b2") (Int 1))
            )
            (Int 0)
    -- sum_b0 + 2*sum_b1 + 4*sum_b2 === sum
    let cReconstructdiff =
          EqC
            110
            ( Add
                ( Add
                    (Var "diff_b0")
                    (Mul (Int 2) (Var "diff_b1"))
                )
                (Mul (Int 4) (Var "diff_b2"))
            )
            (Var "diff")
    -- out === 1 - sum_b2
    let cOutDef =
          EqC
            111
            (Var "out")
            (Sub (Int 1) (Var "diff_b2"))


    -- 3) the program       

    -- all the bindings
    let allBindings =
          [ a,
            b,
            a_b0,
            a_b1,
            b_b0,
            b_b1,
            diff,
            diff_b0,
            diff_b1,
            diff_b2,
            out
          ]

    -- all the constraints
    let allConstraints =
          [ cA_b0_binary,
            cA_b1_binary,
            cReconstructA,
            cB_b0_binary,
            cB_b1_binary,
            cReconstructB,
            cComputediff,
            cdiff_b0_binary,
            cdiff_b1_binary,
            cdiff_b2_binary,
            cReconstructdiff,
            cOutDef
          ]

    -- the program
    let flattenedLessThan2Program =
          Program
            { inputs = [a, b], -- we consider "a" and "b" as top-level inputs
              computationVars = [],
              constraintVars =
                [ a_b0,
                  a_b1,
                  b_b0,
                  b_b1,
                  diff,
                  diff_b0,
                  diff_b1,
                  diff_b2,
                  out
                ],
              computations = [],
              constraints = allConstraints,
              returnVars = [],
              pfRecipExpressions = []
            }

    -- running the analysis
    let finalStates = analyzeProgram flattenedLessThan2Program

    -- checking that input a is [0, 3]
    case Map.lookup "a" finalStates of
      Nothing -> expectationFailure "No state for 'a'"
      Just st ->
        st
          `shouldBe` (VariableState {domain = BoundedValues (Just 0) (Just 3) Set.empty})

    -- checking that input b is [0, 3]
    case Map.lookup "b" finalStates of
      Nothing -> expectationFailure "No state for 'b'"
      Just st ->
        st
          `shouldBe` (VariableState {domain = BoundedValues (Just 0) (Just 3) Set.empty})

    -- checking binary values
    case Map.lookup "a_b0" finalStates of
      Nothing -> expectationFailure "No state for 'a_b0'"
      Just st ->
        st
          `shouldBe` (VariableState {domain = KnownValues (Set.fromList [0, 1])})
    case Map.lookup "a_b1" finalStates of
      Nothing -> expectationFailure "No state for 'a_b1'"
      Just st ->
        st
          `shouldBe` (VariableState {domain = KnownValues (Set.fromList [0, 1])})
    case Map.lookup "b_b0" finalStates of
      Nothing -> expectationFailure "No state for 'b_b0'"
      Just st ->
        st
          `shouldBe` (VariableState {domain = KnownValues (Set.fromList [0, 1])})
    case Map.lookup "b_b1" finalStates of
      Nothing -> expectationFailure "No state for 'b_b1'"
      Just st ->
        st
          `shouldBe` (VariableState {domain = KnownValues (Set.fromList [0, 1])})
    case Map.lookup "diff_b0" finalStates of
      Nothing -> expectationFailure "No state for 'diff_b0'"
      Just st ->
        st
          `shouldBe` (VariableState {domain = KnownValues (Set.fromList [0, 1])})
    case Map.lookup "diff_b1" finalStates of
      Nothing -> expectationFailure "No state for 'diff_b1'"
      Just st ->
        st
          `shouldBe` (VariableState {domain = KnownValues (Set.fromList [0, 1])})
    case Map.lookup "diff_b2" finalStates of
      Nothing -> expectationFailure "No state for 'diff_b2'"
      Just st ->
        st
          `shouldBe` (VariableState {domain = KnownValues (Set.fromList [0, 1])})

    -- checking that diff is [0, 7]
    case Map.lookup "diff" finalStates of
      Nothing -> expectationFailure "No state for 'diff'"
      Just st ->
        st
          `shouldBe` (VariableState {domain = BoundedValues (Just 1) (Just 7) Set.empty})
