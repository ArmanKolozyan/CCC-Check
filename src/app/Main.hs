module Main where

import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import Syntax.Compiler (parseAndCompile)
import Syntax.TagsParser (parseTagsFile)
import Syntax.AST (Program(..), Tag, Binding(..), name, tag)
import ValueAnalysis.Analysis (analyzeProgram)
import ValueAnalysis.Printer (prettyPrintStore)
import BugDetection.BugDetection (detectBugs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [circirFile]           -> analyzeAndDetect circirFile Nothing
        [circirFile, tagsFile] -> analyzeAndDetect circirFile (Just tagsFile)
        _ -> putStrLn "Usage: ccc-check <circir-file> [tags-file]"

-- | applying tags from a tags file to a Program's bindings
applyTags :: Program -> Map.Map String Tag -> Program
applyTags prog tagsMap = prog
    { inputs = map applyTag (inputs prog)
    , computationVars = map applyTag (computationVars prog)
    , constraintVars = map applyTag (constraintVars prog)
    , returnVars = map applyTag (returnVars prog)
    }
  where
    applyTag binding = case Map.lookup (name binding) tagsMap of
        Just t  -> binding { tag = Just t }
        Nothing -> binding

-- | full pipeline: parse IR, apply tags, analyze, detect bugs
analyzeAndDetect :: FilePath -> Maybe FilePath -> IO ()
analyzeAndDetect circirFile maybeTagsFile = do
    content <- readFile circirFile
    case parseAndCompile content of
        Left err -> putStrLn $ "Error: " ++ err
        Right program -> do
            taggedProgram <- case maybeTagsFile of
                Nothing -> return program
                Just tagsPath -> do
                    tagsResult <- parseTagsFile tagsPath
                    case tagsResult of
                        Left err -> error $ "Error parsing tags: " ++ err
                        Right tagsMap -> return (applyTags program tagsMap)
            let store = analyzeProgram taggedProgram
            putStrLn "\n====== Inferred Value Information ======\n"
            prettyPrintStore store
            putStrLn "\n====== Bug Detection Results ======\n"
            case detectBugs taggedProgram Nothing of
                Right () -> putStrLn "No bugs detected."
                Left errors -> mapM_ putStrLn errors
