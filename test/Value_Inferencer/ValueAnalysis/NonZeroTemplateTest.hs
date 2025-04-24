{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.ValueAnalysis.NonZeroTemplateTest (spec) where

import Test.Hspec
import Syntax.AST
import ValueAnalysis.Analysis
import ValueAnalysis.ValueDomain
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = describe "IsZero template test with out=0" $ do
  it "forces in != 0 when out=0" $ do
    -- we use a prime field of size 13 for demonstration

    -- variables in, out, and inv
    let inVar  = Binding { name = "in",  vid = 0, sort = FieldMod p, tag = Just $ MaxValTag 12 }
    let outVar = Binding { name = "out", vid = 1, sort = FieldMod p, tag = Just $ MaxValTag 12 }
    let invVar = Binding { name = "inv", vid = 2, sort = FieldMod p, tag = Just $ MaxValTag 12 }

    -- all constraints from the IsZero template of Circomlib plus "out=0"

    -- out + in*inv = 1
    let constraint1ID = 10
    let c1 = EqC constraint1ID
                  (Add (Var "out") (Mul (Var "in") (Var "inv")))
                  (Int 1)

    -- in * out = 0
    let constraint2ID = 11
    let c2 = EqC constraint2ID
                  (Mul (Var "in") (Var "out"))
                  (Int 0)

    -- out = 0  (the "external" constraint forcing out=0)
    let constraint3ID = 12
    let c3 = EqC constraint3ID
                  (Var "out")
                  (Int 0)

    -- the test program
    let testProgram = Program
          { inputs          = [inVar]     -- 'in' is the input
          , computationVars = []
          , constraintVars  = [outVar, invVar]
          , computations    = []
          , constraints     = [c1, c2, c3]
          , pfRecipExpressions = []
          , returnVars = []
          }

    -- running the analysis
    let finalStates = analyzeProgram testProgram

    -- checking that 'in' has nonZero = True
    case Map.lookup "in" finalStates of
      Nothing -> expectationFailure "No state for 'in'"
      Just st -> do
        -- checking if the domain guarantees non-zero
        isDefinitelyNonZero (domain st) `shouldBe` True
        -- checking that 0 is not a possible value
        couldBeZero (domain st) `shouldBe` False

    -- checking that 'out' is definitely forced to 0
    case Map.lookup "out" finalStates of
      Nothing -> expectationFailure "No state for 'out'"
      Just st -> do
        -- checking if the domain is exactly {0}
        domain st `shouldBe` KnownValues (Set.singleton 0)
        -- checking non-zero status (should be False)
        isDefinitelyNonZero (domain st) `shouldBe` False
        -- checking if it could be zero (should be True)
        couldBeZero (domain st) `shouldBe` True

    -- checking that 'inv' is also non-zero (since in*inv=1)
    case Map.lookup "inv" finalStates of
      Nothing -> expectationFailure "No state for 'inv'"
      Just st -> do
        -- checking if the domain guarantees non-zero
        isDefinitelyNonZero (domain st) `shouldBe` True
        -- Additionally, cjecking that 0 is not a possible value
        couldBeZero (domain st) `shouldBe` False
