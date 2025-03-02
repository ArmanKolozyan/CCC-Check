{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Syntax.Compiler (parseAndCompile)
import Syntax.AST
import Analysis.Analysis (constantProp, isFullyConstrained)

import System.Exit (exitFailure)
import qualified Data.Map as Map
import Data.TypeLevel.HMap (singleton, HMap)
import Domain.Domain
import Lattice.ConstantPropagationLattice

testFile :: String
testFile = "testFiles/eq.circir"

testAnalysis :: IO ()
testAnalysis = do
    content <- readFile testFile
    case parseAndCompile content of
        Left err -> do
            putStrLn $ "Parsing failed with error: " ++ err
            exitFailure
        Right program -> do
            let expectedEnv = Map.fromList [("x", CirCVal (singleton @IntKey (Constant 3)))]
            let env = constantProp program

            if env /= expectedEnv
                then do
                    putStrLn "Constant propagation environment does not match"
                    putStrLn $ "Expected: " ++ show expectedEnv
                    putStrLn $ "Got: " ++ show env
                    exitFailure
                else putStrLn "Constant propagation environment is correct"

            if not (isFullyConstrained env)
                then do
                    putStrLn "isFullyConstrained should return True, but it returned False"
                    exitFailure
                else putStrLn "isFullyConstrained returned True"

main :: IO ()
main = testAnalysis