module Value_Inferencer.Analysis.NonZeroTest (spec) where

import Test.Hspec
import ValueAnalysis.Analysis
import ValueAnalysis.ValueDomain
import Syntax.AST
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "NonZero Rule Tests" $ do
  it "marks variables as non-zero if their product equals a non-zero constant" $ do
    -- we use a prime field of size 17 for demonstration

    -- variables a and b
    let a = Binding { name = "a", vid = 10, sort = FieldMod 17 }
    let b = Binding { name = "b", vid = 11, sort = FieldMod 17 }

    -- 'a * b = 2' and 2 /= 0 (in mod 17)
    let constraintID = 100
    let nonZeroEq = EqC constraintID
                     (Mul (Var "a") (Var "b"))  -- LHS: a*b
                     (Int 2)                    -- RHS: 2

    -- the test program
    let testProgram = Program
          { inputs         = []      
          , computationVars = []
          , constraintVars  = [a, b]
          , computations    = []
          , constraints     = [nonZeroEq]
          , pfRecipExpressions = []
          , returnVars = []
          }

    -- running the analysis
    let finalStates = analyzeProgram testProgram

    -- checking the final VariableState for 'a' and 'b':
    let maybeAState = Map.lookup "a" finalStates
    let maybeBState = Map.lookup "b" finalStates

    case (maybeAState, maybeBState) of
      (Just aState, Just bState) -> do
         -- checking if domain guarantees non-zero
         isDefinitelyNonZero (domain aState) `shouldBe` True
         isDefinitelyNonZero (domain bState) `shouldBe` True

         -- checking if 0 is possibly in the domain (should be False)
         couldBeZero (domain aState) `shouldBe` False
         couldBeZero (domain bState) `shouldBe` False

      _ -> expectationFailure
             "Could not find variable states for 'a' or 'b' in final analysis"
