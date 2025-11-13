module ValueInference.ValueAnalysis.Num2BitsTest (spec) where

import Test.Hspec
import ValueAnalysis.Analysis
import Syntax.AST
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = describe "Value Inference Tests" $ do
  it "Infers correct values for b0, b1, and x" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- input variable
    let x = Binding { name = "x", vid = 0, sort = FieldMod p, tag = Just $ MaxValTag 3 }
    -- intermediate variables b0 and b1
    let b0 = Binding { name = "b0", vid = 1, sort = Bool, tag = Just $ SimpleTag "binary" }
    let b1 = Binding { name = "b1", vid = 2, sort = Bool, tag = Just $ SimpleTag "binary" }

    -- constraints:
    
    -- (b0 * (b0 - 1)) = 0
    let b0_eq = EqC 3 (Mul (Var "b0") (Sub (Var "b0") (Int 1))) (Int 0)

    -- (b1 * (b1 - 1)) = 0
    let b1_eq = EqC 4 (Mul (Var "b1") (Sub (Var "b1") (Int 1))) (Int 0)

    -- (b0 + 2*b1) = x
    let sum_eq = EqC 5 (Add (Var "b0") (Mul (Int 2) (Var "b1"))) (Var "x")

    -- all constraints
    let constraints = [b0_eq, b1_eq, sum_eq]

    -- the test program
    let testProgram = Program
          { inputs = [x]
          , computationVars = []
          , constraintVars = [b0, b1]
          , computations = []
          , constraints = constraints
          , pfRecipExpressions = []
          , returnVars = []
          }

    -- running the analysis
    let inferredStates = analyzeProgram testProgram

    -- checking inferred domain for b0 (should be [0, 1])
    case Map.lookup "b0" inferredStates of
      Nothing -> expectationFailure "No state for 'b0'"
      Just st -> domain st `shouldBe` KnownValues (Set.fromList [0,1])

    -- checking inferred domain for b1 (should be [0, 1])
    case Map.lookup "b1" inferredStates of
      Nothing -> expectationFailure "No state for 'b1'"
      Just st -> domain st `shouldBe` KnownValues (Set.fromList [0,1])

    -- checking inferred domain for x (should be [0, 3] based on sum-of-powers)
    case Map.lookup "x" inferredStates of
      Nothing -> expectationFailure "No state for 'x'"
      Just st -> domain st `shouldBe` BoundedValues (Just 0) (Just 3) Set.empty
