{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module BugDetection.BugDetection (detectBugs) where

import Syntax.AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import ValueAnalysis.VariableState
import ValueAnalysis.ValueDomain
import ValueAnalysis.Analysis (analyzeProgram, inferValues)

{- 
  | This function:
   1) runs 'analyzeProgram' on the Program to get final states 
   2) for each variable in Program, examines its VariableState 
   3) checks whether it is consistent with the variable's declared Sort 
-}

detectBugs :: Program -> Maybe [Binding] -> Either [String] ()
detectBugs program maybeVars =
  let varStates = analyzeProgram program
      allVars = inputs program ++ computationVars program ++ constraintVars program
      vars = fromMaybe allVars maybeVars
      -- gathering errors for each variable based on Sort
      sortErrors = concatMap (checkVariable varStates) vars
      -- gathering division-by-zero errors
      divByZeroErrors = checkPfRecips (pfRecipExpressions program) varStates (buildVarNameToIDMap allVars)
      errors = sortErrors ++ divByZeroErrors
  in if null errors
       then Right ()
       else Left errors

-- | Checks whether one variable's final state is consistent with its declared Sort.
--   Returns either an empty list (no issues) or a list of error messages.
checkVariable
  :: Map String VariableState
  -> Binding
  -> [String]
checkVariable store binding =
  case Map.lookup (name binding) store of
    Nothing -> ["No final state for var `" ++ name binding ++ "`!"]
    Just vState ->
      -- prioritizing checking the tag if it exists
      case tag binding of
        Just t  -> checkTag t vState (name binding)
        -- otherwise we check the sort (i.e., the type)
        Nothing -> checkSort (sort binding) vState (name binding)

-- | Checks that the final VariableState is consistent with the Tag.
--   This acts as a wrapper, dispatching to the domain-specific checker.
checkTag :: Tag -> VariableState -> String -> [String]
checkTag t vs = checkTagDomain t (domain vs)

-- | Checks that a ValueDomain is consistent with its Tag. Handles recursion for arrays.
checkTagDomain :: Tag -> ValueDomain -> String -> [String]
-- array case: checking tag check recursively to elements and default
checkTagDomain t (ArrayDomain elemMap defDom _size) varName =
    let elemErrors = concatMap (\(idx, elemDom) ->
                        checkTagDomain t elemDom (varName ++ "[" ++ show idx ++ "]"))
                     (Map.toList elemMap)
        defErrors = checkTagDomain t defDom (varName ++ "[default]")
    in elemErrors ++ defErrors
-- scalar cases: delegating to specific domain checkers
checkTagDomain (SimpleTag "binary") d varName = checkBooleanDomain d varName
checkTagDomain (SimpleTag "nonzero") d varName = checkNonZeroDomain d varName
checkTagDomain (MaxBitsTag n) d varName       = checkMaxValDomain ((2 ^ n) - 1) d varName
checkTagDomain (MaxValTag n) d varName       = checkMaxValDomain n d varName
-- unhandled tags for scalar domains
checkTagDomain otherTag _ varName              = ["Warning: No check implemented for tag `" ++ show otherTag ++ "` on variable `" ++ varName ++ "`"]  


-- | Checks that the final VariableState is consistent with the Sort.
--   This acts as a wrapper, dispatching to the domain-specific checker.
checkSort :: Sort -> VariableState -> String -> [String]
checkSort s vs = checkSortDomain s (domain vs)

-- | Checks that a ValueDomain is consistent with the Sort. Handles recursion for arrays.
checkSortDomain :: Sort -> ValueDomain -> String -> [String]
-- Array Sort & Array Domain: checking size and recursing on elements
checkSortDomain (ArraySort elemSort expectedSize) (ArrayDomain elemMap defDom actualSize) varName =
    let sizeError = if expectedSize /= actualSize
                    then ["Array `" ++ varName ++ "` size mismatch: declared " ++ show expectedSize ++ ", inferred " ++ show actualSize]
                    else []
        elemErrors = concatMap (\(idx, elemDom) ->
                        checkSortDomain elemSort elemDom (varName ++ "[" ++ show idx ++ "]"))
                     (Map.toList elemMap)
        defErrors = checkSortDomain elemSort defDom (varName ++ "[default]")
    in sizeError ++ elemErrors ++ defErrors    
-- Scalar Sorts vs Scalar Domains
checkSortDomain Bool d varName         = checkBooleanDomain d varName
checkSortDomain (BitVector n) d varName= checkMaxValDomain ((2 ^ n) - 1) d varName
checkSortDomain (FieldMod p') d varName = checkMaxValDomain (p' - 1) d varName
-- Type mismatches
checkSortDomain (ArraySort _ _) _ varName = ["Type mismatch for `" ++ varName ++ "`: expected Array, but found scalar domain."]

-- | Checks that a domain cannot be zero.
checkNonZeroDomain :: ValueDomain -> String -> [String]
checkNonZeroDomain d varName = case d of
    ArrayDomain {} -> ["Type mismatch for `" ++ varName ++ "`: expected NonZero scalar, found Array."]
    _ -> ["Variable `" ++ varName ++ "` tagged 'nonzero' might be zero." | couldBeZero d]

-- Checking Booleans

{- 
   For a boolean variable, the final possibilities can be:

   1) explicit set {0,1}, or subset, or 
   2) an explicit set [some range], but it must not exceed [0..1].

   If 'values' is non-empty, we check that the set is ⊆ {0,1}.
   If 'values' is empty, but we have low_b / upp_b, we check those. 
-}
checkBooleanDomain :: ValueDomain -> String -> [String]
checkBooleanDomain d varName = case d of
        -- known values: checking if subset of {0, 1}
        KnownValues s ->
          let invalidVals = Set.filter (\v -> v /= 0 && v /= 1) s
          in if Set.null invalidVals
             then []
             else ["Boolean variable `" ++ varName ++ "` has values outside {0,1}: " ++ show (Set.toList invalidVals)]

        -- bounded values: checking if bounds are within [0, 1] and no contradictions
        BoundedValues lbM ubM maybeExIntervals ->
          let lbOk = maybe True (>= 0) lbM
              ubOk = maybe True (<= 1) ubM
              -- checking if exclusions make the domain empty within [0, 1]
              isEmptyDueToExclusions = case (lbM, ubM) of
                                         (Just 0, Just 1) -> isExcluded 0 maybeExIntervals p && isExcluded 1 maybeExIntervals p
                                         (Just 0, Just 0) -> isExcluded 0 maybeExIntervals p
                                         (Just 1, Just 1) -> isExcluded 1 maybeExIntervals p
                                         _ -> False -- other bound combinations handled by lbOk/ubOk or don't cover [0,1]
          in (["Boolean variable `" ++ varName ++ "` has lower bound < 0" | not lbOk]) ++
             (["Boolean variable `" ++ varName ++ "` has upper bound > 1" | not ubOk]) ++
             (["Boolean variable `" ++ varName ++ "` has empty domain due to exclusions within [0, 1]" | isEmptyDueToExclusions])
        -- array      
        ArrayDomain {} -> ["Type mismatch for `" ++ varName ++ "`: expected Boolean, found Array."]

-- Checking BitVectors and FieldMods

{- 
   For a variable declared (BitVector n), 
   we want to check that all final possible values are in [0 .. 2^n - 1], 
   or if it has bounds, then the upper bound must not exceed 2^n - 1.
-}
checkMaxValDomain :: Integer -> ValueDomain -> String -> [String]
checkMaxValDomain maxVal d varName = case d of
    KnownValues s ->
        let invalidVals = Set.filter (\v -> v < 0 || v > maxVal) s
        in ["Variable `" ++ varName ++ "` has out-of-range values: " ++ show (Set.toList invalidVals) ++ " (expected [0.." ++ show maxVal ++ "])" | not (Set.null invalidVals)]
    BoundedValues lbM ubM _ -> -- gaps don't cause out-of-range, only emptiness
        let lbOk = maybe True (>= 0) lbM
            ubOk = maybe True (<= maxVal) ubM
        in (["Variable `" ++ varName ++ "` has lower bound < 0" | not lbOk]) ++
           (["Variable `" ++ varName ++ "` has upper bound > " ++ show maxVal | not ubOk])
    ArrayDomain {} -> ["Type mismatch for `" ++ varName ++ "`: expected scalar <= " ++ show maxVal ++ ", found Array."]

-- Checking denominators.

-- | Checks if any PfRecip expression could be zero "at runtime", more precisely :
--   1) if the expression is constrained to be nonZero (via nonZero flag)
--   2) or, 0 is not in the expression's value domain
checkPfRecips :: [Expression] -> Map String VariableState -> Map String Int -> [String]
checkPfRecips denominators store nameToID =
  concatMap checkSingle denominators
  where
    -- converting name-keyed store to ID-keyed for inferValues
    varStatesIntKeys = invertStates store nameToID

    checkSingle expr =
      let inferredDomain = inferValues expr nameToID varStatesIntKeys Nothing
      -- using the functions from ValueDomain to check zero status
      in if isDefinitelyNonZero inferredDomain
         then [] -- guaranteed non-zero, OK
         -- checking if it *could* be zero
         else (["Potential division by zero: Denominator expression `" ++ show expr ++ "` might be zero." | couldBeZero inferredDomain])

-- | Converts String->VariableState to Int->VariableState so we can call inferValues.
invertStates :: Map String VariableState -> Map String Int -> Map Int VariableState
invertStates st nmToID =
  Map.fromList
    [ (nmToID Map.! varName, vState)
    | (varName, vState) <- Map.toList st
    , varName `Map.member` nmToID
    ]