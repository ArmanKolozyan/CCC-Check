{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module CP_Analysis.Analysis.ExpressionsAnalysisTest where

import Syntax.AST
import CPAnalysis.Analysis
import qualified Data.Map as Map
import CPDomain.Domain
import Lattice.ConstantPropagationLattice
import Test.Hspec

spec :: Spec
spec = describe "Expressions Analysis" $ do
    it "adds constants correctly" $ do
        let prog = testProgram [EqC 0 (Var "x") (Add (Int 2) (Int 3))]
            env = constantProp prog
        env `shouldSatisfy` hasExpectedValue "x" (Constant 5)

    it "handles addition with variables and constants" $ do
        let prog = testProgram [EqC 0 (Var "y") (Int 7), EqC 1 (Var "x") (Add (Var "y") (Int 3))]
            env = constantProp prog
        env `shouldSatisfy` hasExpectedValue "x" (Constant 10)

    it "handles addition with unknown variables and constants" $ do
        let prog = testProgram [EqC 0 (Var "x") (Add (Var "y") (Int 3))]
            env = constantProp prog
        env `shouldSatisfy` hasExpectedValue "x" Top  

    it "handles multiplication with constants" $ do
        let prog = testProgram [EqC 0 (Var "x") (Mul (Int 4) (Int 5))]
            env = constantProp prog
        env `shouldSatisfy` hasExpectedValue "x" (Constant 20)

    it "handles multiplication with a variable and a constant" $ do
        let prog = testProgram [EqC 0 (Var "y") (Int 6), EqC 1 (Var "x") (Mul (Var "y") (Int 2))]
            env = constantProp prog
        env `shouldSatisfy` hasExpectedValue "x" (Constant 12)    

    it "handles subtraction with two constants" $ do
        let prog = testProgram [EqC 0 (Var "x") (Sub (Int 10) (Int 4))]
            env = constantProp prog
        env `shouldSatisfy` hasExpectedValue "x" (Constant 6)     

    it "handles conditional expressions" $ do
        let prog = testProgram [EqC 0 (Var "x") (Ite (Int 0) (Int 42) (Int 7))]
            env = constantProp prog
        env `shouldSatisfy` hasExpectedValue "x" Top

-- | Helper function to check whether, for the given variable,
-- the expected value is present in the environment.
hasExpectedValue :: String -> CP Integer -> Env -> Bool
hasExpectedValue var expected env =
    case Map.lookup var env of
        Just val -> selectCP val == expected
        Nothing  -> False

-- | Helper function to create a test program
testProgram :: [Constraint] -> Program
testProgram = Program [] [] [] [] [] []      
