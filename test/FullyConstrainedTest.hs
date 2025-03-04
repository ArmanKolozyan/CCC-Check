{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module FullyConstrainedTest where

import Syntax.Compiler (parseAndCompile)
import Analysis.Analysis (constantProp, isFullyConstrained)

import qualified Data.Map as Map
import Data.TypeLevel.HMap (singleton)
import Domain.Domain
import Lattice.ConstantPropagationLattice

testFile :: String
testFile = "testFiles/eq.circir"

runFullyConstrainedTest :: IO Bool
runFullyConstrainedTest = do
    content <- readFile testFile
    case parseAndCompile content of
        Left err -> do
            putStrLn $ "Parsing failed with error: " ++ err
            return False
        Right program -> do
            let expectedEnv = Map.fromList [("x", CirCVal (singleton @IntKey (Constant 3)))]
            let env = constantProp program

            -- First check: does the environment match 'expectedEnv'?
            if env /= expectedEnv
                then do
                    putStrLn "Constant propagation environment does not match"
                    putStrLn $ "Expected: " ++ show expectedEnv
                    putStrLn $ "Got: " ++ show env
                    return False
                else do
                    putStrLn "Constant propagation environment is correct"

                    -- Second check: is the environment fully constrained?
                    if not (isFullyConstrained env)
                        then do
                            putStrLn "isFullyConstrained should return True, but it returned False"
                            return False
                        else do
                            putStrLn "isFullyConstrained returned True"
                            return True