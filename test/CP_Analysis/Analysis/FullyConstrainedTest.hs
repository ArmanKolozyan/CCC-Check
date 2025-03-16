{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module CP_Analysis.Analysis.FullyConstrainedTest where

import Syntax.Compiler (parseAndCompile)
import Analysis.Analysis (constantProp, isFullyConstrained)
import qualified Data.Map as Map
import Data.TypeLevel.HMap (singleton)
import Domain.Domain
import Lattice.ConstantPropagationLattice
import Test.Hspec

spec :: Spec
spec = describe "Fully Constrained Test" $ do
    it "checks if all variables are fully constrained to a constant" $ do
        content <- readFile "testFiles/eq.circir"
        case parseAndCompile content of
            Left err -> expectationFailure $ "Parsing failed: " ++ err
            Right program -> do
                let expectedEnv = Map.fromList [("x", CirCVal (singleton @IntKey (Constant 3)))]
                    env = constantProp program
                env `shouldBe` expectedEnv
                isFullyConstrained env `shouldBe` True