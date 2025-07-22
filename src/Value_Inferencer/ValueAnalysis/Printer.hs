
module ValueAnalysis.Printer (prettyPrintStore, formatDomain) where

import ValueAnalysis.ValueDomain (ValueDomain(..))
import ValueAnalysis.VariableState (VariableState(..))
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)



-- | Nicely prints the analysis results.
prettyPrintStore :: Map String VariableState -> IO ()
prettyPrintStore store = do
    mapM_ printVariable (Map.toList store)
  where
    printVariable (varName, vState) = do
        putStrLn $ "Variable: " ++ varName
        putStr "- Inferred Domain: "
        putStrLn $ formatDomain (domain vState)

-- Helper to format ValueDomain for printing
formatDomain :: ValueDomain -> String
formatDomain (KnownValues s) = "{" ++ intercalate ", " (map show (Set.toList s)) ++ "}"
formatDomain (BoundedValues lbM ubM currentGaps) =
    let boundsStr = case (lbM, ubM) of
                      (Just lb, Just ub) -> "[" ++ show lb ++ ", " ++ show ub ++ "]"
                      (Just lb, Nothing) -> "[" ++ show lb ++ ", inf)"
                      (Nothing, Just ub) -> "(-inf, " ++ show ub ++ "]"
                      (Nothing, Nothing) -> "Unknown"
        exclusionsStr = if Set.null currentGaps then "" 
                        else " excluding " ++ formatIntervals (Set.toList currentGaps)
    in boundsStr ++ exclusionsStr
formatDomain (ArrayDomain elems defaultDom size) =
    let elementsStr = if Map.null elems then ""
                      else " specific: " ++ intercalate ", " 
                           (map (\(i, dom) -> show i ++ ":" ++ formatDomain dom) (Map.toList elems))
        defaultStr = "default: " ++ formatDomain defaultDom
    in "Array[" ++ show size ++ "](" ++ defaultStr ++ elementsStr ++ ")"

-- Helper to format list of intervals
formatIntervals :: [(Integer, Integer)] -> String
formatIntervals intervals = "[" ++ intercalate ", " (map formatSingleInterval intervals) ++ "]"
  where
    formatSingleInterval (l, u)
      | l == u    = show l
      | otherwise = show l ++ ".." ++ show u