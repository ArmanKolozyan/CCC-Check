module Syntax.IteParseTest where

import Syntax.Compiler (parseAndCompile)
import Syntax.AST
import Test.Hspec

spec :: Spec
spec = describe "ITE Parsing Test" $ do
    it "parses ITE expressions correctly" $ do
        content <- readFile "testFiles/ite.circir"
        case parseAndCompile content of
            Left err -> expectationFailure $ "Parsing failed: " ++ err
            Right program -> computations program `shouldContain` [Ite (Var "sel") (Var "a") (Var "b")]