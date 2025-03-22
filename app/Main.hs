module Main where    

import Syntax.Compiler (parseAndCompile)
import ValueAnalysis.Analysis
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)

testFile :: String
testFile = "testFiles/num2bits.circir" 

main :: IO ()
main = do
    content <- readFile testFile
    case parseAndCompile content of
        Left err -> putStrLn $ "Error: " ++ err  
        Right program -> do
            let store = analyzeProgram program
            putStrLn "\n====== Inferred Value Information ======\n"
            prettyPrintStore store

-- | Nicely prints the analysis results.
prettyPrintStore :: Map String VariableState -> IO ()
prettyPrintStore store = do
    mapM_ printVariable (Map.toList store)
  where
    printVariable (varName, VariableState values low_b upp_b _) = do
        putStrLn $ "Variable: " ++ varName
        putStr "- Inferred Values: "
        case determineState values low_b upp_b of
            Left explicitValues -> putStrLn $ "{" ++ intercalate ", " (map show (Set.toList explicitValues)) ++ "}"
            Right (Just lb, Just ub) -> putStrLn $ "[" ++ show lb ++ ", " ++ show ub ++ "]"
            Right _ -> putStrLn "Unknown"
        putStrLn ""

    -- Determines whether to display explicit values, bounds, or unknown.
    determineState :: Set Integer -> Maybe Integer -> Maybe Integer -> Either (Set Integer) (Maybe Integer, Maybe Integer)
    determineState vals lb ub
        | not (Set.null vals) = Left vals  -- explicit values 
        | otherwise           = Right (lb, ub)  -- otherwise, we display bounds or unknown