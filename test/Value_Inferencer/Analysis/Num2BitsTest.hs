module Value_Inferencer.Analysis.Num2BitsTest (spec) where

import Test.Hspec
import ValueAnalysis.Analysis
import Syntax.AST
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = describe "Value Inference Tests" $ do
  it "Infers correct values for b0, b1, and x" $ do
    -- input variable
    let x = Binding { name = "x", vid = 0, sort = FieldMod 5 }  -- Assume prime field mod 5

    -- intermediate variables b0 and b1
    let b0 = Binding { name = "b0", vid = 1, sort = Bool }
    let b1 = Binding { name = "b1", vid = 2, sort = Bool }

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
          }

    -- analysis
    let inferredStates = analyzeProgram testProgram

    -- checking inferred values for b0
    Map.lookup "b0" inferredStates `shouldBe` Just (VariableState { values = Set.fromList [0,1], low_b = Nothing, upp_b = Nothing, nonZero = False})

    -- checking inferred values for b1
    Map.lookup "b1" inferredStates `shouldBe` Just (VariableState { values = Set.fromList [0,1], low_b = Nothing, upp_b = Nothing, nonZero = False })

    -- checking inferred values for x (should be in range [0,3])
    Map.lookup "x" inferredStates `shouldBe` Just (VariableState { values = Set.empty, low_b = Just 0, upp_b = Just 3, nonZero = False })
