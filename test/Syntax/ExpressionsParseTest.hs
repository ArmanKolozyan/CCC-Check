{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Syntax.ExpressionsParseTest where

import Syntax.AST
import Syntax.Compiler (parseAndCompile)
import Test.Hspec

spec :: Spec
spec = describe "Expression Parsing" $ do
    it "parses and compiles correctly" $ do
        content <- readFile "testFiles/expressions.circir"
        case parseAndCompile content of
            Left err -> expectationFailure $ "Parsing or compilation failed: " ++ err
            Right program -> computations program `shouldContain` [expectedExpr]
  where
    expectedExpr = Ite
        (And (Or (Gt (Var "x") (Var "y")) (Lt (Var "x") (Int 10)))
             (Or (Gte (Var "y") (Int 5))
                 (Or (Lte (Var "x") (Int 2)) (Not (Var "x")))))
        (Add (Sub (Var "x") (Int 2)) (Var "y"))
        (Mul (Var "x") (Var "y"))