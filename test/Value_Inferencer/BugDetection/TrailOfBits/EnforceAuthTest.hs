{-# LANGUAGE OverloadedStrings #-}

-- Test for flattened EnforceAuth template
module Value_Inferencer.BugDetection.TrailOfBits.EnforceAuthTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- Test case for the flattened EnforceAuth template from https://blog.trailofbits.com/2024/01/02/tag-youre-it-signal-tagging-in-circom/
spec :: Spec
spec = describe "Flattened EnforceAuth template analysis test" $ do
  it "correctly infers all signals are binary and finds no errors" $ do

    -- using BN254 as prime field for demonstration (though not strictly necessary as all are Bool)
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- variables (all declared Bool based on binary tags)
    let var_valueA       = Binding { name = "valueA",       vid = 0, sort = Bool }
    let var_valueB       = Binding { name = "valueB",       vid = 1, sort = Bool }
    let var_authSucceeded = Binding { name = "authSucceeded", vid = 2, sort = FieldMod p }
    let var_spendsA      = Binding { name = "spendsA",      vid = 3, sort = Bool }
    let var_spendsB      = Binding { name = "spendsB",      vid = 4, sort = Bool }
    let var_authRequired = Binding { name = "authRequired", vid = 5, sort = Bool }

    -- computations (representing <== assignments)

    -- signal spendsA <== valueA
    -- WE REMOVED THE ToBinary CALL FROM THE ORIGINAL CODE AND REPLACED IT BY 
    -- BINARY CONSTRAINTS (SEE BELOW) FOR EXPERIMENTAL PURPOSES. 
    -- PLEASE SEE ToBinaryTest.hs FOR ALL THE DETAILS.
    let comp_spendsA = Assign "spendsA" (Var "valueA")

    -- signal spendsB <== valueB;
    -- WE REMOVED THE ToBinary CALL FROM THE ORIGINAL CODE AND REPLACED IT BY 
    -- BINARY CONSTRAINTS (SEE BELOW) FOR EXPERIMENTAL PURPOSES. 
    -- PLEASE SEE ToBinaryTest.hs FOR ALL THE DETAILS.
    let comp_spendsB = Assign "spendsB" (Var "valueB")

    -- signal authRequired <== OR()(spendsA, spendsB);
    -- authRequired <== spendsA + spendsB - spendsA*spendsB;
    let comp_authRequired = Assign "authRequired" (Sub (Add (Var "spendsA") (Var "spendsB"))
                                                        (Mul (Var "spendsA") (Var "spendsB")))

    -- constraints

    -- valueA*(valueA-1) == 0
    let constraint1ID = 10
    let c1 = EqC constraint1ID (Mul (Var "valueA") (Sub (Var "valueA") (Int 1))) (Int 0)

    -- valueB*(valueB-1) == 0
    let constraint2ID = 11
    let c2 = EqC constraint2ID (Mul (Var "valueB") (Sub (Var "valueB") (Int 1))) (Int 0)

    -- spendsA <== valueA
    let constraint3ID = 12
    let c3 = EqC constraint3ID (Var "spendsA") (Var "valueA")

    -- spendsB <== valueB
    let constraint4ID = 13
    let c4 = EqC constraint4ID (Var "spendsB") (Var "valueB")

    -- authRequired <== OR()(spendsA, spendsB);
    let constraint5ID = 14
    let c5 = EqC constraint5ID (Var "authRequired") (Sub (Add (Var "spendsA") (Var "spendsB"))
                                                    (Mul (Var "spendsA") (Var "spendsB")))

    -- (1 - authSucceeded) * authRequired === 0;
    let constraint6ID = 15
    let c6 = EqC constraint6ID (Mul (Sub (Int 1) (Var "authSucceeded")) (Var "authRequired")) (Int 0)

    -- the test program
    let testProgram = Program
          { inputs          = [var_valueA, var_valueB, var_authSucceeded]
          , computationVars = [var_spendsA, var_spendsB, var_authRequired]
          , constraintVars  = []
          , computations    = [comp_spendsA, comp_spendsB, comp_authRequired]
          , constraints     = [c1, c2, c3, c4, c5, c6]
          , pfRecipExpressions = []
          , returnVars = []
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assertions

    -- Checking that detectBugs returned Right () (no errors)
    bugResult `shouldSatisfy` isRight
    bugResult `shouldBe` Right ()
