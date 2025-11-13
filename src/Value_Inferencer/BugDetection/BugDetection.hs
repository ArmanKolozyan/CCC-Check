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
import Data.Bits ((.&.))
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
      nameToIDMap = buildVarNameToIDMap allVars
      -- converting name-keyed store to ID-keyed for inferValues
      varStatesIntKeys = invertStates varStates nameToIDMap


      -- gathering errors for each variable based on Sort
      sortErrors = concatMap (checkVariable varStates) vars
      -- gathering division-by-zero errors
      divByZeroErrors = checkPfRecips (pfRecipExpressions program) varStates nameToIDMap varStatesIntKeys
      -- gathering array access out-of-bounds errors
      arrayAccessErrors = checkArrayAccesses (constraints program) nameToIDMap varStatesIntKeys

      errors = sortErrors ++ divByZeroErrors ++ arrayAccessErrors
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
checkTagDomain t (ArrayDomain elemMap defDom size) varName =
    let elemErrors = concatMap (\(idx, elemDom) ->
                        checkTagDomain t elemDom (varName ++ "[" ++ show idx ++ "]"))
                     (Map.toList elemMap)
        -- only checking the default domain if the element map does not cover all indices
        defErrors = if fromIntegral (Map.size elemMap) == size
                    then []
                    else checkTagDomain t defDom (varName ++ "[default]")
    in elemErrors ++ defErrors
-- scalar cases: delegating to specific domain checkers
checkTagDomain (SimpleTag "binary") d varName = checkBooleanDomain d varName
checkTagDomain (SimpleTag "nonzero") d varName = checkNonZeroDomain d varName
checkTagDomain (SimpleTag "powerof2") d varName = checkPowerOf2Domain d varName
checkTagDomain (MaxBitsTag n) d varName       = checkMaxValDomain ((2 ^ n) - 1) d varName
checkTagDomain (MaxValTag n) d varName       = checkMaxValDomain n d varName
checkTagDomain (MinValTag n) d varName        = checkMinValDomain n d varName
checkTagDomain (MaxAbsTag n) d varName        = checkMaxAbsDomain n d varName
checkTagDomain (MaxBitsAbsTag n) d varName    = checkMaxBitsAbsDomain n d varName
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
        -- only checking the default domain if the element map does not cover all indices
        defErrors = if fromIntegral (Map.size elemMap) == actualSize
                    then []
                    else checkSortDomain elemSort defDom (varName ++ "[default]")
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

-- | Checks that a domain has a minimum value.
checkMinValDomain :: Integer -> ValueDomain -> String -> [String]
checkMinValDomain minVal d varName = case d of
    KnownValues s ->
        let invalidVals = Set.filter (< minVal) s
        in ["Variable `" ++ varName ++ "` has out-of-range values: " ++ show (Set.toList invalidVals) ++ " (expected >= " ++ show minVal ++ ")" | not (Set.null invalidVals)]
    BoundedValues lbM ubM _ ->
        let lbOk = maybe False (>= minVal) lbM  -- lower bound must exist and be >= minVal
        in ["Variable `" ++ varName ++ "` has lower bound < " ++ show minVal | not lbOk]
    ArrayDomain {} -> ["Type mismatch for `" ++ varName ++ "`: expected scalar >= " ++ show minVal ++ ", found Array."]

-- | Checks that a domain has absolute values within bounds.
checkMaxAbsDomain :: Integer -> ValueDomain -> String -> [String]
checkMaxAbsDomain maxAbs d varName = case d of
    KnownValues s ->
        let invalidVals = Set.filter (\v -> abs v > maxAbs) s
        in ["Variable `" ++ varName ++ "` has out-of-range values: " ++ show (Set.toList invalidVals) ++ " (expected |value| <= " ++ show maxAbs ++ ")" | not (Set.null invalidVals)]
    BoundedValues lbM ubM _ ->
        let lbOk = maybe True (>= negate maxAbs) lbM
            ubOk = maybe True (<= maxAbs) ubM
        in (["Variable `" ++ varName ++ "` has lower bound < " ++ show (negate maxAbs) | not lbOk]) ++
           (["Variable `" ++ varName ++ "` has upper bound > " ++ show maxAbs | not ubOk])
    ArrayDomain {} -> ["Type mismatch for `" ++ varName ++ "`: expected scalar with |value| <= " ++ show maxAbs ++ ", found Array."]

-- | Checks that a domain has absolute values within bit bounds.
checkMaxBitsAbsDomain :: Integer -> ValueDomain -> String -> [String]
checkMaxBitsAbsDomain n d varName =
    let maxAbs = 2 ^ n
    in checkMaxAbsDomain maxAbs d varName

-- | Checks that a domain contains only powers of 2.
checkPowerOf2Domain :: ValueDomain -> String -> [String]
checkPowerOf2Domain d varName = case d of
    KnownValues s ->
        let invalidVals = Set.filter (not . isPowerOf2') s
        in ["Variable `" ++ varName ++ "` has values that are not powers of 2: " ++ show (Set.toList invalidVals) | not (Set.null invalidVals)]
    BoundedValues lbM ubM gaps ->
        checkBoundedPowerOf2 lbM ubM gaps varName
    ArrayDomain {} -> ["Type mismatch for `" ++ varName ++ "`: expected scalar power of 2, found Array."]
  where
    isPowerOf2' :: Integer -> Bool
    isPowerOf2' n = n > 0 && (n .&. (n - 1)) == 0

    -- checking for bounded domains
    checkBoundedPowerOf2 :: Maybe Integer -> Maybe Integer -> Set.Set (Integer, Integer) -> String -> [String]
    checkBoundedPowerOf2 lbM ubM gaps varName =
        let lb = fromMaybe 0 lbM
            ub = fromMaybe (p - 1) ubM
        in if lb < 0 then
            -- negative values cannot be powers of 2
            ["Variable `" ++ varName ++ "` has negative lower bound " ++ show lb ++ ", but powers of 2 must be positive"]
           else if lb == 0 && not (isExcluded 0 gaps p) then
            -- zero is not a power of 2
            ["Variable `" ++ varName ++ "` may contain 0, which is not a power of 2"]
           else
            -- generating powers of 2 within the bounds
            let maxExponent = ceiling (logBase 2 (fromIntegral p))
                allPowersOfTwo = takeWhile (< p) [2^i | i <- [0..maxExponent]]
                powersInRange = filter (\x -> x >= lb && x <= ub && not (isExcluded x gaps p)) allPowersOfTwo

                -- For bounded domains, we check exhaustively if the range is small enough.
                -- For large ranges, to maintain soundness, we report that verification is not feasible.
                rangeSize = ub - lb + 1

            in if null powersInRange then
                ["Variable `" ++ varName ++ "` has bounds [" ++ show lb ++ ".." ++ show ub ++ "] which contain no powers of 2"]
               else if rangeSize <= 1000 then
                -- for small ranges, we check exhaustively
                let nonPowersInRange = filter (\x -> not (isPowerOf2' x) && not (isExcluded x gaps p)) [lb..ub]
                in (["Variable `" ++ varName ++ "` may contain non-power-of-2 values: " ++ show (take 10 nonPowersInRange) ++
                          (if length nonPowersInRange > 10 then " and " ++ show (length nonPowersInRange - 10) ++ " more" else "") | not (null nonPowersInRange)])
               else
                -- For large ranges, we cannot exhaustively check without risking performance issues.
                -- To be sound, we must report that the constraint may be violated.
                ["Variable `" ++ varName ++ "` has large range [" ++ show lb ++ ".." ++ show ub ++ 
                 "] (size " ++ show rangeSize ++ ") - cannot efficiently verify power-of-2 constraint"]

-- Checking denominators.

-- | Checks if any PfRecip expression could be zero "at runtime", more precisely :
--   1) if the expression is constrained to be nonZero (via nonZero flag)
--   2) or, 0 is not in the expression's value domain
checkPfRecips :: [Expression] -> Map String VariableState -> Map String Int -> Map Int VariableState -> [String]
checkPfRecips denominators store nameToID varStatesIntKeys =
  concatMap checkSingle denominators
  where
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


-- | Collects all ArrayStore and ArraySelect expressions from a list of constraints.
collectArrayAccesses :: [Constraint] -> [Expression]
collectArrayAccesses = concatMap collectFromConstraint
  where
    collectFromConstraint :: Constraint -> [Expression]
    collectFromConstraint (EqC _ e1 e2) = collectFromExpr e1 ++ collectFromExpr e2
    collectFromConstraint (AndC _ cs)   = concatMap collectFromConstraint cs
    collectFromConstraint (OrC _ cs)    = concatMap collectFromConstraint cs
    collectFromConstraint (NotC _ c)    = collectFromConstraint c

    collectFromExpr :: Expression -> [Expression]
    -- collecting ArrayStore and recursing
    collectFromExpr store@(ArrayStore arr idx val) =
        [store] ++ collectFromExpr arr ++ collectFromExpr idx ++ collectFromExpr val
    -- collecting ArraySelect and recursing
    collectFromExpr select@(ArraySelect arr idx) =
        [select] ++ collectFromExpr arr ++ collectFromExpr idx
    -- standard recursion for other expression types
    collectFromExpr (Add e1 e2)     = collectFromExpr e1 ++ collectFromExpr e2
    collectFromExpr (Sub e1 e2)     = collectFromExpr e1 ++ collectFromExpr e2
    collectFromExpr (Mul e1 e2)     = collectFromExpr e1 ++ collectFromExpr e2
    collectFromExpr (Ite c t e)     = collectFromExpr c ++ collectFromExpr t ++ collectFromExpr e
    collectFromExpr (Eq e1 e2)      = collectFromExpr e1 ++ collectFromExpr e2
    collectFromExpr (Gt e1 e2)      = collectFromExpr e1 ++ collectFromExpr e2
    collectFromExpr (Lt e1 e2)      = collectFromExpr e1 ++ collectFromExpr e2
    collectFromExpr (Gte e1 e2)     = collectFromExpr e1 ++ collectFromExpr e2
    collectFromExpr (Lte e1 e2)     = collectFromExpr e1 ++ collectFromExpr e2
    collectFromExpr (And es)        = concatMap collectFromExpr es
    collectFromExpr (Or es)         = concatMap collectFromExpr es
    collectFromExpr (Not e)         = collectFromExpr e
    collectFromExpr (PfRecip e)     = collectFromExpr e
    collectFromExpr (Let bs body)   = concatMap (collectFromExpr . snd) bs ++ collectFromExpr body
    collectFromExpr (ArrayLiteral elems _) = concatMap collectFromExpr elems
    collectFromExpr (ArraySparseLiteral indexedExprs defExpr _ _) =
        concatMap (collectFromExpr . snd) indexedExprs ++ collectFromExpr defExpr
    collectFromExpr (ArrayConstruct exprs _) = concatMap collectFromExpr exprs
    collectFromExpr (ArrayFill valExpr _ _) = collectFromExpr valExpr
    collectFromExpr _               = [] -- Var, Int, FieldConst

-- | Checks ArrayStore and ArraySelect expressions for potential out-of-bounds access
--   using known indices based on the final inferred variable states.
checkArrayAccesses :: [Constraint] -> Map String Int -> Map Int VariableState -> [String]
checkArrayAccesses programConstraints nameToIDMap varStatesIntKeys =
    let arrayAccessExprs = collectArrayAccesses programConstraints
    in concatMap checkSingle arrayAccessExprs
    where
      checkSingle :: Expression -> [String]
      checkSingle accessExpr =
          -- extracting array and index expressions based on the type of access
          let (arrExp, idxExp, accessType) = case accessExpr of
                ArrayStore arr idx _ -> (arr, idx, "ArrayStore")
                ArraySelect arr idx  -> (arr, idx, "ArraySelect")
                -- should not happen, but just in case
                _                    -> error "checkArrayAccesses called with non-array access expression"
          in
          -- inferring domains using the final states
          let arrDom = inferValues arrExp nameToIDMap varStatesIntKeys Nothing
              idxDom = inferValues idxExp nameToIDMap varStatesIntKeys Nothing
          in
          -- performing check only if array domain is known and index domain is a set of known values
          case (arrDom, idxDom) of
               (ArrayDomain _ _ size, KnownValues idxSet) ->
                   -- foltering for indices that are out of the valid range [0, size-1]
                   let outOfBoundsIndices = Set.filter (\idx -> idx < 0 || idx >= size) idxSet
                   in -- generating error message if any out-of-bounds indices are found
                      ["Potential " ++ accessType ++ " out-of-bounds: Index(es) " ++ show (Set.toList outOfBoundsIndices) ++
                       " used with array of size " ++ show size ++ " in expression `" ++ show accessExpr ++ "`"
                       | not (Set.null outOfBoundsIndices)]
               -- cannot perform bounds check if array size is unknown or index is not a known set
               _ -> []