module Main where

import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import Control.DeepSeq (deepseq, NFData)
import Syntax.Compiler (parseAndCompile)
import Syntax.TagsParser (parseTagsFile)
import qualified Data.IntMap.Strict as IntMap
import Syntax.AST (Program(..), Tag, Binding(..), name, tag)
import ValueAnalysis.Analysis (precomputeAnalysis, runAnalysis, PrecomputedAnalysis(..), transformIDToNames)
import ValueAnalysis.Printer (prettyPrintStore)
import BugDetection.BugDetection (detectBugsWithStore)

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

-- | Run an IO action N times, forcing evaluation each time, and return
--   the total elapsed time in milliseconds and the result from the last run.
benchmarkN :: NFData a => Int -> IO a -> IO (Double, a)
benchmarkN n action = do
    -- Warm-up run
    warmup <- action
    warmup `deepseq` return ()
    -- Timed runs
    t0 <- getCurrentTime
    result <- go n undefined
    t1 <- getCurrentTime
    let totalMs = realToFrac (nominalDiffTimeToSeconds (diffUTCTime t1 t0)) * 1000
    return (totalMs / fromIntegral n, result)
  where
    go 0 lastResult = return lastResult
    go i _ = do
        r <- action
        r `deepseq` go (i - 1) r

-- | Choose iteration count: for fast operations use more iterations
--   to amortize getCurrentTime overhead (~2μs per call).
chooseIters :: Double -> Int
chooseIters estimateMs
    | estimateMs < 0.1  = 1000  -- < 100μs: run 1000x
    | estimateMs < 1.0  = 100   -- < 1ms: run 100x
    | estimateMs < 10.0 = 10    -- < 10ms: run 10x
    | otherwise         = 1

-- | full pipeline: parse IR, apply tags, analyze, detect bugs
analyzeAndDetect :: FilePath -> Maybe FilePath -> IO ()
analyzeAndDetect circirFile maybeTagsFile = do
    t0 <- getCurrentTime
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
            -- Force evaluation of the tagged program to complete parsing
            taggedProgram `deepseq` return ()
            t1 <- getCurrentTime

            -- Precompute nameToID etc. once (outside timed section)
            let pa = precomputeAnalysis taggedProgram
            pa `deepseq` return ()

            -- First pass: rough estimate for calibration (run 10x to amortize overhead)
            (roughMs, _) <- benchmarkN 10 $
                return $! runAnalysis pa
            let iters = chooseIters roughMs

            -- Accurate measurement with multiple iterations
            (analysisMs, intMapStore) <- benchmarkN iters $
                return $! runAnalysis pa

            let nameToID = paNameToID pa
            (bugdetMs, bugResult) <- benchmarkN iters $
                return $! detectBugsWithStore taggedProgram Nothing nameToID intMapStore

            t3 <- getCurrentTime
            -- Convert to String-keyed map only for display
            let idToName = IntMap.fromList [(vid, nm) | (nm, vid) <- Map.toList nameToID]
                store = transformIDToNames idToName intMapStore
            putStrLn "\n====== Inferred Value Information ======\n"
            prettyPrintStore store
            putStrLn "\n====== Bug Detection Results ======\n"
            case bugResult of
                Right () -> putStrLn "No bugs detected."
                Left errors -> mapErr putStrLn errors
            let toMs t = realToFrac (nominalDiffTimeToSeconds t) * 1000 :: Double
                parseMs = toMs (diffUTCTime t1 t0)
                totalMs = parseMs + analysisMs + bugdetMs
                fmt x = let s = show (fromIntegral (round (x * 10000) :: Int) / 10000 :: Double)
                         in s
            putStrLn $ "\n====== Timing ======"
            putStrLn $ "  Parsing:       " ++ fmt parseMs ++ " ms"
            putStrLn $ "  Analysis:      " ++ fmt analysisMs ++ " ms  (" ++ show iters ++ " iterations)"
            putStrLn $ "  Bug detection: " ++ fmt bugdetMs ++ " ms  (" ++ show iters ++ " iterations)"
            putStrLn $ "  Total:         " ++ fmt totalMs ++ " ms"
  where
    mapErr = mapM_
