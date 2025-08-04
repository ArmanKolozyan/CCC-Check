{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module CP_Analysis.Analysis.FullyConstrainedTest where

import Syntax.Compiler (parseAndCompile)
import CPAnalysis.Analysis (constantProp, isFullyConstrained)
import qualified Data.Map as Map
import Data.TypeLevel.HMap (singleton)
import CPDomain.Domain
import Lattice.ConstantPropagationLattice
import Test.Hspec

spec :: Spec
spec = describe "Fully Constrained Test" $ do
    it "checks if all variables are fully constrained to a constant" $ do
        content <- readFile "test/circir-testFiles/eq.circir"
        case parseAndCompile content of
            Left err -> expectationFailure $ "Parsing failed: " ++ err
            Right program -> do
                let expectedEnv = Map.fromList [("x", CirCVal (singleton @IntKey (Constant 3)))]
                    env = constantProp program
                env `shouldBe` expectedEnv
                isFullyConstrained env `shouldBe` True

    it "checks if output is constrained to a constant given the appropriate input" $ do
        content <- readFile "test/circir-testFiles/outputConstrained.circir"
        case parseAndCompile content of
            Left err -> expectationFailure $ "Parsing failed: " ++ err
            Right program -> do
                let expectedOutput = CirCVal (singleton @IntKey (Constant 0))
                    env = constantProp program
                Map.lookup "o" env `shouldBe` Just expectedOutput

    it "checks if output is not constrained to a constant given the appropriate input" $ do
        content <- readFile "test/circir-testFiles/outputNotConstrained.circir"
        case parseAndCompile content of
            Left err -> expectationFailure $ "Parsing failed: " ++ err
            Right program -> do
                let expectedOutput = CirCVal (singleton @IntKey Top)
                    env = constantProp program
                Map.lookup "o" env `shouldBe` Just expectedOutput


