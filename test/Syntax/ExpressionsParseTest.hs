{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Syntax.ExpressionsParseTest where

import Syntax.AST
import Syntax.Compiler (parseAndCompile)

testFile :: String
testFile = "testFiles/expressions.circir"

runExpressionParsingTest :: IO Bool
runExpressionParsingTest = do
  content <- readFile testFile
  case parseAndCompile content of
    Left err -> do
      putStrLn $ "FAILURE: Parsing or compilation failed with error: " ++ err
      return False
    Right program -> do
      let expectedExpr =
            Ite
              (And
                  (Or 
                      (Gt (Var "x") (Var "y")) 
                      (Lt (Var "x") (Int 10))
                  )
                  (Or
                      (Gte (Var "y") (Int 5))
                      (Or
                          (Lte (Var "x") (Int 2))
                          (Not (Var "x"))
                      )
                  )
              )
              (Add (Sub (Var "x") (Int 2)) (Var "y"))
              (Mul (Var "x") (Var "y"))

      let actualComputations = computations program

      if any (== expectedExpr) actualComputations
        then do
          putStrLn "SUCCESS: Found the expected expression in computations."
          return True
        else do
          putStrLn "FAILURE: Did not find the expected expression in computations."
          putStrLn $ "Got: " ++ show actualComputations
          return False
