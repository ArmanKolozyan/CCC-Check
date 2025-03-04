{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module IteParseTest where

import Syntax.Compiler (parseAndCompile)
import Syntax.AST        

testFile :: String
testFile = "testFiles/ite.circir"

runIteParsingTest :: IO Bool
runIteParsingTest = do
    content <- readFile testFile
    case parseAndCompile content of
      Left err -> do
          putStrLn $ "FAILURE: Parsing or compilation failed with error: " ++ err
          return False
      Right program -> do
          -- we want to confirm that the Program’s computations contain 
          -- an 'Ite (Var \"sel\") (Var \"a\") (Var \"b\")' expression.
          let expected = Ite (Var "sel") (Var "a") (Var "b")
              actualComputations = computations program

          if any (== expected) actualComputations
            then do
              putStrLn "SUCCESS: Found Ite (Var \"sel\") (Var \"a\") (Var \"b\") in computations."
              return True
            else do
              putStrLn "FAILURE: Did not find the expected Ite expression in computations."
              putStrLn $ "Got: " ++ show actualComputations
              return False