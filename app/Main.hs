module Main where    

import Syntax.Compiler
import Syntax.AST
import System.IO (readFile)

testFile :: String
testFile = "test.circir" 

main :: IO ()
main = do
    content <- readFile testFile
    case parseAndCompile content of
        Left err -> putStrLn $ "Error: " ++ err  
        Right program -> print program

