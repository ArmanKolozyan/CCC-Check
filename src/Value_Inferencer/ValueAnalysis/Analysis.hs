{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}

module ValueAnalysis.Analysis (analyzeProgram, VariableState(..), updateValues, initVarState, ValueDomain(..), analyzeFromFile, inferValues) where

import Syntax.AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as MapMerge
import qualified Data.Set as Set
import Data.Sequence (Seq, (|>), viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)
import Syntax.Compiler (parseAndCompile)
import ValueAnalysis.UserRules
import ValueAnalysis.VariableState
import ValueAnalysis.ValueDomain
import ValueAnalysis.Printer

import Debug.Trace (trace)

--------------------------
-- 1) Variable State Representation
--------------------------

-- Moved to separate file (VariableState.hs)

--------------------------
-- 2) Initializing Variable States
--------------------------

-- Moved to separate file (VariableState.hs)

--------------------------
-- 3) Mapping Variables to Constraints
--------------------------

-- | Collects all variable IDs from a constraint.
collectVarsFromConstraint :: Map String Int -> Constraint -> [Int]
collectVarsFromConstraint nameToID (EqC _ e1 e2) =
    collectVarsFromExpr nameToID e1 ++ collectVarsFromExpr nameToID e2
collectVarsFromConstraint nameToID (AndC _ cs) =
    concatMap (collectVarsFromConstraint nameToID) cs
collectVarsFromConstraint nameToID (OrC _ cs) =
    concatMap (collectVarsFromConstraint nameToID) cs
collectVarsFromConstraint nameToID (NotC _ c) =
    collectVarsFromConstraint nameToID c

-- | Collects all variable IDs from an expression, using nameToID map.
collectVarsFromExpr :: Map String Int -> Expression -> [Int]
collectVarsFromExpr nameToID (Var name) = case Map.lookup name nameToID of
    Just vID -> [vID]
    Nothing  -> error $ "Variable name not found: " ++ name
collectVarsFromExpr _ (Int _) = []
collectVarsFromExpr nameToID (Add e1 e2) = collectVarsFromExpr nameToID e1 ++ collectVarsFromExpr nameToID e2
collectVarsFromExpr nameToID (Sub e1 e2) = collectVarsFromExpr nameToID e1 ++ collectVarsFromExpr nameToID e2
collectVarsFromExpr nameToID (Mul e1 e2) = collectVarsFromExpr nameToID e1 ++ collectVarsFromExpr nameToID e2
collectVarsFromExpr nameToID (Ite e1 e2 e3) = collectVarsFromExpr nameToID e1 ++ collectVarsFromExpr nameToID e2 ++ collectVarsFromExpr nameToID e3
collectVarsFromExpr nameToID (Eq e1 e2) = collectVarsFromExpr nameToID e1 ++ collectVarsFromExpr nameToID e2
collectVarsFromExpr nameToID (Gt e1 e2) = collectVarsFromExpr nameToID e1 ++ collectVarsFromExpr nameToID e2
collectVarsFromExpr nameToID (Lt e1 e2) = collectVarsFromExpr nameToID e1 ++ collectVarsFromExpr nameToID e2
collectVarsFromExpr nameToID (Gte e1 e2) = collectVarsFromExpr nameToID e1 ++ collectVarsFromExpr nameToID e2
collectVarsFromExpr nameToID (Lte e1 e2) = collectVarsFromExpr nameToID e1 ++ collectVarsFromExpr nameToID e2
collectVarsFromExpr nameToID (And es) = concatMap (collectVarsFromExpr nameToID) es
collectVarsFromExpr nameToID (Or es) = concatMap (collectVarsFromExpr nameToID) es
collectVarsFromExpr nameToID (Not e) = collectVarsFromExpr nameToID e
collectVarsFromExpr nameToID (PfRecip e) = collectVarsFromExpr nameToID e

-- | Builds a variable-to-constraints mapping.
buildVarToConstraints :: Map String Int -> [Constraint] -> Map Int [Int]
buildVarToConstraints nameToID constraints =
  let pairs = concatMap (extract nameToID) constraints
  in Map.fromListWith (++) pairs
  where
    extract :: Map String Int -> Constraint -> [(Int, [Int])]
    extract nameToID (EqC cid e1 e2) =
        [(v, [cid]) | v <- collectVarsFromExpr nameToID e1 ++ collectVarsFromExpr nameToID e2]
    extract nameToID (AndC cid cs) = concatMap (extract nameToID) cs
    extract nameToID (OrC cid cs) = concatMap (extract nameToID) cs
    extract nameToID (NotC cid c) = extract nameToID c

--------------------------
-- 4) Setting Up the Processing Queue
--------------------------

-- | Initializes the constraint queue with all constraints.
initializeQueue :: [Constraint] -> Seq Int
initializeQueue constraints = Seq.fromList (map getConstraintID constraints)

getConstraintID :: Constraint -> Int
getConstraintID (EqC cid _ _) = cid
getConstraintID (AndC cid _) = cid
getConstraintID (OrC cid _) = cid
getConstraintID (NotC cid _) = cid

--------------------------
-- 5) Value Inferencing
--------------------------

-- | Updates values of a variable and checks consistency.
--   It does this by intersecting the existing domain with the new domain information.
--   Returns Left String on contradiction, Right VariableState on success.
updateValues :: VariableState -> ValueDomain -> Either String VariableState
updateValues oldState newDomainInfo =
  -- 1. getting the current domain from the old state
  let currentDomain = domain oldState
  -- 2. intersecting the current domain with the new information
  in case intersectDomains currentDomain newDomainInfo of
       -- 3a. if intersection fails (contradiction), we return the error
       Left errMsg -> Left errMsg
       -- 3b. if intersection succeeds, we create a new VariableState with the updated domain
       Right updatedDomain -> Right (oldState { domain = updatedDomain })

-- | Computes the union (least upper bound) of two value domains.
joinDomains :: ValueDomain -> ValueDomain -> ValueDomain
joinDomains d1 d2 = case (d1, d2) of
  -- KnownValues: taking the union of sets
  (KnownValues s1, KnownValues s2) -> KnownValues (Set.union s1 s2)

  -- BoundedValues: taking the min of lower bounds, max of upper bounds,
  -- and empty gaps (over-approximation).
  (BoundedValues lb1M ub1M _, BoundedValues lb2M ub2M _) ->
    let joinedLb = combineBounds min lb1M lb2M -- min of lower bounds
        joinedUb = combineBounds max ub1M ub2M -- max of upper bounds
    in BoundedValues joinedLb joinedUb Set.empty -- empty gaps

  -- KnownValues and BoundedValues:
  -- converting KnownValues to an equivalent (potentially imprecise) BoundedValues
  -- and then joining the two BoundedValues
  (KnownValues s, b@(BoundedValues {})) ->
    if Set.null s
    then b -- joining with empty set changes nothing
    else let minVal = Set.findMin s
             maxVal = Set.findMax s
             -- representing as BoundedValues [minVal, maxVal] (imprecise if non-contiguous)
             kvAsBv = BoundedValues (Just minVal) (Just maxVal) Set.empty
         in joinDomains kvAsBv b
  -- symmetric case       
  (b@(BoundedValues {}), KnownValues s) -> joinDomains (KnownValues s) b

  -- ArrayDomains:
  -- Requires sizes to match. 
  -- Joining default domains. 
  -- Joining element domains element-wise, considering the default domain
  -- when an element is missing from one map.
  (ArrayDomain elems1 def1 size1, ArrayDomain elems2 def2 size2) ->
    if size1 /= size2
    then defaultValueDomain -- TODO: or error?
    else
      let joinedDef = joinDomains def1 def2
          -- using Map.merge for sound joining of element domains
          joinedElems = MapMerge.merge
            (MapMerge.mapMissing (\_k d1_i -> joinDomains d1_i def2)) -- key only in elems1: joining d1_i with def2
            (MapMerge.mapMissing (\_k d2_i -> joinDomains def1 d2_i)) -- key only in elems2: joining def1 with d2_i
            (MapMerge.zipWithMatched (\_k d1_i d2_i -> joinDomains d1_i d2_i)) -- key in both: joining d1_i with d2_i
            elems1
            elems2
      in ArrayDomain joinedElems joinedDef size1

  -- joining incompatible types (e.g., Array and Scalar)
  _ -> defaultValueDomain

-- | Recursively infers possible values of an expression
-- Receives optional map for local (let) bindings.
inferValues :: Expression -> Map String Int -> Map Int VariableState -> Maybe (Map String ValueDomain) -> ValueDomain
inferValues (Int c) _ _ _ = KnownValues (Set.singleton c)
inferValues (FieldConst i p) _ _ _ = KnownValues $ Set.singleton (i `mod` p)

-- Variable lookup: checking local bindings first, then global
inferValues (Var xName) nameToID varStates maybeLocalBindings =
  -- if we found the variable state, we return the domain from the state
  case maybeLocalBindings >>= Map.lookup xName of
     Just localDomain -> localDomain -- found in local let-bindings
     Nothing -> -- not found locally, looking up globally
      case Map.lookup xName nameToID of
          -- variable known but no state? we return default domain
          Just varID -> maybe defaultValueDomain domain (Map.lookup varID varStates)
          -- variable name not found? we return default domain
          -- TODO: beter error geven denk ik
          Nothing -> defaultValueDomain

-- Let expression: evaluating bindings, then body in augmented scope
inferValues (Let bindings bodyExp) nameToID varStates maybeOuterLocalBindings =
  -- 1. evaluating binding expressions in the *current* scope (which includes outer locals)
  let newLocalBindings = Map.fromList $ map
        (\(name, expr) -> (name, inferValues expr nameToID varStates maybeOuterLocalBindings))
        bindings

  -- 2. combining new local bindings with outer local bindings (new shadows outer)
      combinedLocalBindings = case maybeOuterLocalBindings of
                                Just outerMap -> Map.union newLocalBindings outerMap
                                Nothing -> newLocalBindings

  -- 3. evaluating the body expression in the new combined local scope
  in inferValues bodyExp nameToID varStates (Just combinedLocalBindings)

inferValues (Add e1 e2) nameToID varStates maybeLocalBindings =
    let d1 = inferValues e1 nameToID varStates maybeLocalBindings
        d2 = inferValues e2 nameToID varStates maybeLocalBindings
    in case (d1, d2) of
      -- we enumerate values, but should not be a problem since KnownValues are supposed to be
      -- small sets
       (KnownValues s1, KnownValues s2) ->
         KnownValues $ Set.fromList [ (v1 + v2) `mod` p | v1 <- Set.toList s1, v2 <- Set.toList s2 ]

       (BoundedValues (Just lb1) (Just ub1) _, KnownValues s2) | not (Set.null s2) ->
         let minSum = minimum $ Set.map (\v2 -> (lb1 + v2) `mod` p) s2
             maxSum = maximum $ Set.map (\v2 -> (ub1 + v2) `mod` p) s2
         in if minSum <= maxSum then
              -- no wrap-around: result is [minSum, maxSum]
              -- We drop original exclusions, which is sound as we only
              -- lose precision with this (we over-approximate).
              -- TODO: maybe we can keep the exclusions in a smart way? Yes should be doable.
              BoundedValues (Just minSum) (Just maxSum) Set.empty
            else
              -- wrap-around: we represent as [0, p-1] excluding the gap.
              let gaps = getWrapAroundExclusion maxSum minSum p
              in BoundedValues (Just 0) (Just (p - 1)) gaps

       (KnownValues s1, BoundedValues (Just lb2) (Just ub2) _) | not (Set.null s1) ->
         -- symmetric
         let minSum = minimum $ Set.map (\v1 -> (v1 + lb2) `mod` p) s1
             maxSum = maximum $ Set.map (\v1 -> (v1 + ub2) `mod` p) s1
         in if minSum <= maxSum then
              BoundedValues (Just minSum) (Just maxSum) Set.empty
            else
              let gaps = getWrapAroundExclusion maxSum minSum p
              in BoundedValues (Just 0) (Just (p - 1)) gaps

       (BoundedValues (Just lb1) (Just ub1) _, BoundedValues (Just lb2) (Just ub2) _) ->
         let newLb = (lb1 + lb2) `mod` p
             newUb = (ub1 + ub2) `mod` p
         in if newLb <= newUb then
              -- no wrap-around: Result is [newLb, newUb]
              -- We drop original exclusions.
              BoundedValues (Just newLb) (Just newUb) Set.empty
            else
              -- wrap-around: we represent as [0, p-1] excluding the gap.
              let gaps = getWrapAroundExclusion newUb newLb p
              in BoundedValues (Just 0) (Just (p - 1)) gaps

       _ -> defaultValueDomain

inferValues (Sub e1 e2) nameToID varStates maybeLocalBindings =
  let d1 = inferValues e1 nameToID varStates maybeLocalBindings
      d2 = inferValues e2 nameToID varStates maybeLocalBindings
  in case (d1, d2) of
       (KnownValues s1, KnownValues s2) ->
         KnownValues $ Set.fromList [ (v1 - v2) `mod` p | v1 <- Set.toList s1, v2 <- Set.toList s2 ]

       (BoundedValues (Just lb1) (Just ub1) gaps1, KnownValues s2) | not (Set.null s2) ->
         let minSub = minimum $ Set.map (\v2 -> (lb1 - v2 + p) `mod` p) s2
             maxSub = maximum $ Set.map (\v2 -> (ub1 - v2 + p) `mod` p) s2
             wrapAround = minSub > maxSub
             -- mapping exclusions from gaps1: applying mapExclusionSubKnown to each interval in gaps1 
             -- and we union the results
             mappedExclusions = Set.unions $ Set.map (\interval -> mapExclusionSubKnown interval s2 p) gaps1
             -- combining with wrap-around gap
             finalExclusions =
               if wrapAround then
                 let wrapGapSet = getWrapAroundExclusion maxSub minSub p
                 in Set.union wrapGapSet mappedExclusions
               else
                 mappedExclusions
             (finalLb, finalUb) = if wrapAround then (0, p-1) else (minSub, maxSub)
         in BoundedValues (Just finalLb) (Just finalUb) finalExclusions

       (KnownValues s1, BoundedValues (Just lb2) (Just ub2) gaps2) | not (Set.null s1) ->
         let minSub = minimum $ Set.map (\v1 -> (v1 - ub2 + p) `mod` p) s1 -- v1 - max(d2)
             maxSub = maximum $ Set.map (\v1 -> (v1 - lb2 + p) `mod` p) s1 -- v1 - min(d2)
             wrapAround = minSub > maxSub
             -- mapping exclusions from gaps2: applying mapExclusionKnownSub to each interval in gaps2 
             -- and we union results
             mappedExclusions = Set.unions $ Set.map (\interval -> mapExclusionKnownSub s1 interval p) gaps2

             finalExclusions =
               if wrapAround then
                 let wrapGapSet = getWrapAroundExclusion maxSub minSub p
                 in Set.union wrapGapSet mappedExclusions
               else
                 mappedExclusions
             (finalLb, finalUb) = if wrapAround then (0, p-1) else (minSub, maxSub)
         in BoundedValues (Just finalLb) (Just finalUb) finalExclusions

       -- keeping original exclusions is very complex here, so we drop them
       (BoundedValues (Just lb1) (Just ub1) _, BoundedValues (Just lb2) (Just ub2) _) ->
         -- we first calculate number of elements in each interval based on bounds
         let 
          -- helper to count values in [lb, ub] mod p
          numValues lb ub = (ub - lb + p) `mod` p
          n1 = numValues lb1 ub1
          n2 = numValues lb2 ub2
         in -- then we check using Cauchy-Davenport condition: 
            -- if n1 + n2 - 1 >= p, the result might cover the full field.
            -- https://arxiv.org/pdf/1202.1816  
            if n1 + n2 - 1 >= p then
              -- we soundly over-approximate the result as the full field
              -- we lose precision (original bounds and gaps), but ensure soundness
              BoundedValues (Just 0) (Just (p - 1)) Set.empty
            else
              let newLb = (lb1 - ub2) `mod` p
                  newUb = (ub1 - lb2) `mod` p
              in if newLb <= newUb then
                    BoundedValues (Just newLb) (Just newUb) Set.empty
                  else
                    let gaps = getWrapAroundExclusion newUb newLb p
                    in BoundedValues (Just 0) (Just (p - 1)) gaps

       _ -> defaultValueDomain


inferValues (Mul e1 e2) nameToID varStates maybeLocalBindings =
  let d1 = inferValues e1 nameToID varStates maybeLocalBindings
      d2 = inferValues e2 nameToID varStates maybeLocalBindings

  in case (d1, d2) of
       -- if either is certainly zero, the result is certainly zero
       _ | isCertainlyZeroDomain d1 || isCertainlyZeroDomain d2 -> KnownValues (Set.singleton 0)

       (KnownValues s1, KnownValues s2) ->
         KnownValues $ Set.fromList [ (v1 * v2) `mod` p | v1 <- Set.toList s1, v2 <- Set.toList s2 ]

       -- bounded * known (non-zero)
       (BoundedValues (Just lb1) (Just ub1) _, KnownValues s2) | not (Set.null s2) && not (Set.member 0 s2) ->
         -- checking if the interval operand might contain zero
         if couldBeZero d1 then
           -- Multiplying an interval that might contain zero (e.g., [0, 2] mod p)
           -- by a non-zero constant `k` can result in a set that is not easily
           -- representable as a single interval `[l, u]` or even `[0, p-1]` excluding a gap.
           -- Example: `[0, 2] * 10 mod 17` = `{0, 1, 2} * 10 mod 17`
           -- = `{0*10, 1*10, 2*10} mod 17` = `{0, 10, 20} mod 17` = `{0, 10, 3}`.
           -- This resulting set `{0, 3, 10}` cannot be efficiently captured by our BoundedValues structure
           -- without significant loss of precision (e.g., becoming [0, 16]).
           -- Returning `defaultValueDomain` (unknown) is a safe over-approximation,
           -- by which we acknowledge the limitations of our analysis.
           -- TODO: think more about this, maybe we can represent this easily?
           defaultValueDomain
         else
           -- interval does not contain 0. 
           -- multiplying bounds by min/max of known constants
           let min_k = minimum s2
               max_k = maximum s2
               corners = [ (lb1 * min_k) `mod` p, (lb1 * max_k) `mod` p,
                           (ub1 * min_k) `mod` p, (ub1 * max_k) `mod` p ]
               minProd = minimum corners
               maxProd = maximum corners
           in if minProd <= maxProd then
                -- we drop original exclusions as mapping them through multiplication is complex
                BoundedValues (Just minProd) (Just maxProd) Set.empty
              else
                -- we wrap around
                let gaps = getWrapAroundExclusion maxProd minProd p
                in BoundedValues (Just 0) (Just (p - 1)) gaps

       -- known (non-zero) * bounded
       (KnownValues s1, BoundedValues (Just lb2) (Just ub2) maybeEx2) | not (Set.null s1) && not (Set.member 0 s1) ->
         -- symmetric case
         if couldBeZero d1 then
           -- same reasoning as above applies
           defaultValueDomain
         else
           let min_k = minimum s1
               max_k = maximum s1
               corners = [ (min_k * lb2) `mod` p, (max_k * lb2) `mod` p,
                           (min_k * ub2) `mod` p, (max_k * ub2) `mod` p ]
               minProd = minimum corners
               maxProd = maximum corners
           in if minProd <= maxProd then
                BoundedValues (Just minProd) (Just maxProd) Set.empty
              else
                let gaps = getWrapAroundExclusion maxProd minProd p
                in BoundedValues (Just 0) (Just (p - 1)) gaps

       -- if zero is involved with intervals (and not handled by the non-zero constant cases above), result is 
       -- the default domain as calculating the exact domain efficiently is difficult. 
       (KnownValues s1, BoundedValues lb2M ub2M _) | Set.member 0 s1 -> defaultValueDomain
       (BoundedValues lb1M ub1M ex1M, KnownValues s2) | Set.member 0 s2 -> defaultValueDomain
       (BoundedValues lb1M ub1M ex1M, BoundedValues lb2M ub2M _) | couldBeZero d1 || couldBeZero d2 -> defaultValueDomain

       -- bounded * bounded (where neither contains zero)
       (BoundedValues (Just lb1) (Just ub1) _, BoundedValues (Just lb2) (Just ub2) _) ->
          -- approximation: multiplying bounds, finding min/max mod p
          let corners = [ (lb1*lb2) `mod` p, (lb1*ub2) `mod` p,
                          (ub1*lb2) `mod` p, (ub1*ub2) `mod` p ]
              minProd = minimum corners
              maxProd = maximum corners
          in if minProd <= maxProd then
               BoundedValues (Just minProd) (Just maxProd) Set.empty
             else
               -- wrapping around
               let gaps = getWrapAroundExclusion maxProd minProd p
               in BoundedValues (Just 0) (Just (p - 1)) gaps

       _ -> defaultValueDomain

inferValues (PfRecip e) nameToID varStates maybeLocalBindings = inferValues e nameToID varStates maybeLocalBindings

inferValues (Ite cond eThen eElse) nameToID varStates maybeLocalBindings =
  let dThen = inferValues eThen nameToID varStates maybeLocalBindings
      dElse = inferValues eElse nameToID varStates maybeLocalBindings
      -- TODO: Could potentially use cond to refine which branch is taken
  in joinDomains dThen dElse 

-- Array Literal: e.g., #l(e1, e2, ...)
inferValues (ArrayLiteral elems sort) nameToID varStates maybeLocalBindings =
  let elemDoms = map (\e -> inferValues e nameToID varStates maybeLocalBindings) elems
      size = fromIntegral $ length elems
      -- creating map from index to domain
      elemMap = Map.fromList $ zip [0..] elemDoms
      -- all elements are specified
  in ArrayDomain elemMap defaultValueDomain size

-- ArraySparseLiteral: #a[(idx1, val1), (idx2, val2)...] default size sort
inferValues (ArraySparseLiteral indexedExprs defaultExpr size _sort) nameToID varStates maybeLocalBindings =
  -- inferring domain for the default value
  let defDom = inferValues defaultExpr nameToID varStates maybeLocalBindings
      -- inferring domains for explicitly indexed values
      elemMap = Map.fromList $ map (\(idx, expr) -> (idx, inferValues expr nameToID varStates maybeLocalBindings)) indexedExprs
  in ArrayDomain elemMap defDom size

-- ArrayConstruct: (array val1 val2 ...) sort
inferValues (ArrayConstruct exprs _sort) nameToID varStates maybeLocalBindings =
  -- inferring domains for each element
  let elemDoms = map (\e -> inferValues e nameToID varStates maybeLocalBindings) exprs
      size = fromIntegral $ length exprs
      -- creating map from index to domain
      elemMap = Map.fromList $ zip [0..] elemDoms
      -- all elements are specified
  in ArrayDomain elemMap defaultValueDomain size

-- Array Select: select(arr, idx)
inferValues (ArraySelect arrExp idxExp) nameToID varStates maybeLocalBindings =
  let arrDom = inferValues arrExp nameToID varStates maybeLocalBindings
      idxDom = inferValues idxExp nameToID varStates maybeLocalBindings
  in case arrDom of
       ArrayDomain elemMap defDom size ->
          -- the set of possible valid indices
         let possibleIndices = getPossibleIndices idxDom size
         in if Set.null possibleIndices
            then
              -- If no valid index is possible (e.g., out of bounds, excluded by gaps),
              -- the result is uncertain. 
              -- Returning default domain.
              defaultValueDomain
            else
              -- getting the domain for each possible index
              let domainsToJoin = map (\idx -> Map.findWithDefault defDom idx elemMap) (Set.toList possibleIndices)
              -- Joining the domains of all possible elements.
              -- Foldr1 requires a non-empty list, which Set.null check guarantees.
              in foldr1 joinDomains domainsToJoin

       -- selecting from something not an array => invalid
       _ -> defaultValueDomain

-- Array Store: store(arr, idx, val)
inferValues (ArrayStore arrExp idxExp valExp) nameToID varStates maybeLocalBindings =
  let arrDom = inferValues arrExp nameToID varStates maybeLocalBindings
      idxDom = inferValues idxExp nameToID varStates maybeLocalBindings
      valDom = inferValues valExp nameToID varStates maybeLocalBindings
  in case arrDom of
       ArrayDomain elemMap defDom size ->
         case idxDom of

           -- index is a single known value
           KnownValues idxSet | Set.size idxSet == 1 ->
             let idx = Set.findMin idxSet
             in if idx >= 0 && idx < size then
                  -- creating new array domain with updated element map
                  let newElemMap = Map.insert idx valDom elemMap
                  in ArrayDomain newElemMap defDom size
                else
                  -- index out of bounds, store has no effect?
                  -- TODO: error?
                  arrDom

           -- index is known set of multiple values or a range/unknown
           _ ->
             -- Storing at an unknown index. Over-approximation:
             -- 1. joining the stored value's domain with the default domain
             let newDefDom = joinDomains defDom valDom
             -- 2. joining the stored value's domain with every explicitly tracked element domain
                 newElemMap = Map.map (joinDomains valDom) elemMap
             in ArrayDomain newElemMap newDefDom size

       -- storing into something not an array => invalid
       _ -> defaultValueDomain

-- ArrayFill: fill(value, sort, size)
inferValues (ArrayFill valExpr _sort size) nameToID varStates maybeLocalBindings =
  -- inferring domain for the fill value
  let fillDom = inferValues valExpr nameToID varStates maybeLocalBindings
      -- all elements initially have this domain; it becomes the default!
      -- the specific element map starts empty
  in ArrayDomain Map.empty fillDom size

inferValues _ _ _ _ = defaultValueDomain -- TODO: Handle other cases properly

-- Helper function to map an exclusion interval through subtraction by a set of constants
-- Input: (l1, u1) from gaps1, Set s2, modulus p
-- Output: Set of resulting exclusion intervals {(l1-v2, u1-v2) | v2 in s2}
mapExclusionSubKnown :: (Integer, Integer) -> Set.Set Integer -> Integer -> Set.Set (Integer, Integer)
mapExclusionSubKnown (l1, u1) s2 p =
  Set.map (\v2 ->
    let newL = (l1 - v2 + p) `mod` p
        newU = (u1 - v2 + p) `mod` p
    -- Note: The resulting interval (newL, newU) might wrap around itself (newL > newU).
    in (newL, newU)
  ) s2

-- Helper function to map an exclusion interval through subtraction *from* a set of constants
-- Input: Set s1, (l2, u2) from gaps2, modulus p
-- Output: Set of resulting exclusion intervals {(v1-u2, v1-l2) | v1 in s1}
mapExclusionKnownSub :: Set.Set Integer -> (Integer, Integer) -> Integer -> Set.Set (Integer, Integer)
mapExclusionKnownSub s1 (l2, u2) p =
  Set.map (\v1 ->
    let newL = (v1 - u2 + p) `mod` p -- subtracting the upper bound of the gap gives the lower bound of the result gap
        newU = (v1 - l2 + p) `mod` p -- subtracting the lower bound of the gap gives the upper bound of the result gap
    in (newL, newU)
  ) s1

-- In modular arithmetic (mod p), an interval can "wrap around" the modulus.
-- If the calculated lower bound `newLb` is GREATER than the calculated upper bound `newUb`,
-- it signifies wrap-around. The actual values represented are [newLb, p-1] U [0, newUb].
-- Instead of representing this union directly (which our BoundedValues doesn't support),
-- we represent it as the entire field [0, p-1] but EXCLUDING the "gap" interval
-- from (newUb + 1) to (newLb - 1) modulo p.
--
-- Example: We calculate `A - B` where A is [1, 2], B is [1, 2], and p=17.
-- The rule is `[a, b] - [c, d] = [a-d, b-c] mod p`.
-- 1. Calculate `newLb = (a - d) mod p = (1 - 2) mod 17 = -1 mod 17 = 16`.
-- 2. Calculate `newUb = (b - c) mod p = (2 - 1) mod 17 = 1 mod 17 = 1`. => [16, 1] 
-- 3. Check: Is `newLb > newUb`? Yes, `16 > 1`. This signifies wrap-around.
-- 4. The actual values are `[newLb, p-1] U [0, newUb]`, which is `[16, 16] U [0, 1]`.
--    The set of possible values is {16, 0, 1}.
-- 5. Representation: We represent this as the field `[0, p-1]` but EXCLUDING the "gap".
--    The gap is `[(newUb + 1) mod p, (newLb - 1 + p) mod p]`.
--    `gapStart = (1 + 1) mod 17 = 2`.
--    `gapEnd = (16 - 1 + 17) mod 17 = 15`.
--    The excluded gap interval is `[2, 15]`.
--    The result is stored as `BoundedValues (Just 0) (Just (p-1)) (Just [(2, 15)])`.
--    TODO: We assume here that ValueDomain uses excluded intervals. Check if we respect this everywhere.

-- Helper to calculate the bounds of the gap interval during wrap-around.
-- Returns the interval (gapStart, gapEnd) representing excluded values.
calculateGapInterval :: Integer -> Integer -> Integer -> (Integer, Integer)
calculateGapInterval ubWrap lbWrap modulus =
    let gapStart = (ubWrap + 1) `mod` modulus
        gapEnd   = (lbWrap - 1 + modulus) `mod` modulus
    in (gapStart, gapEnd)

getWrapAroundExclusion :: Integer -> Integer -> Integer -> Set.Set (Integer, Integer)
getWrapAroundExclusion ubWrap lbWrap modulus =
    let (gapStart, gapEnd) = calculateGapInterval ubWrap lbWrap modulus
    in if (gapStart - 1 + modulus) `mod` modulus == gapEnd then
         Set.empty -- no effective exclusion
       else
         Set.singleton (gapStart, gapEnd)

-- | Helper function: Extracts (x - c) terms from a multiplication expression.
extractRootFactors :: Expression -> Maybe (String, [Integer])
extractRootFactors (Mul e1 e2) =
    case (extractRootFactors e1, extractRootFactors e2) of
        (Just (x1, cs1), Just (x2, cs2)) | x1 == x2 -> Just (x1, cs1 ++ cs2)
        _ -> Nothing
extractRootFactors (Sub (Var xName) (Int c)) = Just (xName, [c])
extractRootFactors (Var xName) = Just (xName, [0]) -- is equivalent to (x - 0)
extractRootFactors _ = Nothing

zeroOne :: Int -> String -> String -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)
zeroOne _ xName yName nameToID oldVarStates = do
   xID <- lookupVarID xName nameToID
   yID <- lookupVarID yName nameToID

   xSt <- lookupVarState xID oldVarStates
   ySt <- lookupVarState yID oldVarStates

   -- checking if x is already known to be in [0,1]
   if isIn01 xSt then do
     -- if so, setting y to [0,1]
     newY <- updateValues ySt (BoundedValues (Just 0) (Just 1) Set.empty)
     let changedY = newY /= ySt
         newMapY  = if changedY
                    then Map.insert yID newY oldVarStates
                    else oldVarStates
     pure (changedY, newMapY)

   -- otherwise, checking if y is already known to be in [0,1]
   else if isIn01 ySt then do
     -- if so, setting x to [0,1]
     newX <- updateValues xSt (BoundedValues (Just 0) (Just 1) Set.empty)
     let changedX = newX /= xSt
         newMapX  = if changedX
                    then Map.insert xID newX oldVarStates
                    else oldVarStates
     pure (changedX, newMapX)

   else
     -- no update if neither is already in [0,1].
     pure (False, oldVarStates)

-- Helper function: checks whether a VariableState is restricted to [0,1].
isIn01 :: VariableState -> Bool
isIn01 st = case domain st of
    KnownValues s -> Set.isSubsetOf s (Set.fromList [0, 1])
    BoundedValues lbM ubM gaps ->
        let -- checking whether bounds are within [0, 1]
            boundsOk = case (lbM, ubM) of
                         (Just lb, Just ub) -> lb >= 0 && ub <= 1
                         _ -> False -- Cannot be exactly [0, 1]

            -- checking whether exclusions don't make it impossible
            exclusionsOk = case (lbM, ubM) of
                             (Just 0, Just 1) -> not (isExcluded 0 gaps p && isExcluded 1 gaps p) 
                             (Just 0, Just 0) -> not (isExcluded 0 gaps p) 
                             (Just 1, Just 1) -> not (isExcluded 1 gaps p)
                             _ -> True -- if bounds aren't exactly [0,0], [1,1] or [0,1], exclusions don't invalidate the [0,1] check

        in boundsOk && exclusionsOk
        
-- | Applies an "interesting" constraint to update variable states.
-- TODO: OrC, ... !!
analyzeConstraint :: Constraint -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)


-- | Boolean OR Rule: out = a + b - a*b
--   If a and b are binary, then out must be binary.
analyzeConstraint (EqC _ (Var outName) (Sub (Add (Var aName1) (Var bName1)) (Mul (Var aName2) (Var bName2)))) nameToID varStates =
  -- checking if variables match for OR pattern
  if aName1 == aName2 && bName1 == bName2 then
    -- it is the OR pattern, now checking if inputs are binary
    if isBinaryVar nameToID varStates aName1 && isBinaryVar nameToID varStates bName1 then
      -- applying the OR rule
      constrainVarToBinary outName nameToID varStates
    else
      Right (False, varStates)
  else
    Right (False, varStates)

-- | Boolean XOR Rule: out = a + b - 2*a*b
--   If a and b are binary, then out must be binary.
analyzeConstraint (EqC _ (Var outName) (Sub (Add (Var aName1) (Var bName1)) (Mul (Int 2) (Mul (Var aName2) (Var bName2))))) nameToID varStates =
  -- checking if variables match for XOR pattern
  if aName1 == aName2 && bName1 == bName2 then
    if isBinaryVar nameToID varStates aName1 && isBinaryVar nameToID varStates bName1 then
      -- applying XOR rule
      constrainVarToBinary outName nameToID varStates
    else
      Right (False, varStates)
  else
    Right (False, varStates)

-- | Boolean NOT Rule: out = 1 - in (equivalent to 1 + in - 2*in for binary in)
--   If in is binary, then out must be binary.
analyzeConstraint (EqC _ (Var outName) (Sub (Int 1) (Var inName))) nameToID varStates
  -- necessary check: if the guard fails, this clause does not match, and pattern matching continues.
  | isBinaryVar nameToID varStates inName
  = constrainVarToBinary outName nameToID varStates

-- | Boolean NOR Rule: out = a*b + 1 - a - b (equivalent to 1 - OR(a,b))
--   If a and b are binary, then out must be binary.
analyzeConstraint (EqC _ (Var outName) (Sub (Sub (Add (Mul (Var aName1) (Var bName1)) (Int 1)) (Var aName2)) (Var bName2))) nameToID varStates =
  -- checking if variables match for NOR pattern
  trace "dddd" $ if aName1 == aName2 && bName1 == bName2 then
    if isBinaryVar nameToID varStates aName1 && isBinaryVar nameToID varStates bName1 then
      -- applying NOR rule
      constrainVarToBinary outName nameToID varStates 
    else
      Right (False, varStates)
  else
    Right (False, varStates)

-- EQUALITY Rule: x = y
analyzeConstraint (EqC _ (Var xName) e) nameToID varStates =
  case e of
    (Var yName) ->
      case (Map.lookup xName nameToID, Map.lookup yName nameToID) of
        (Just xID, Just yID) ->
          case (Map.lookup xID varStates, Map.lookup yID varStates) of
            (Just xState, Just yState) ->
              -- propagating information in both directions using updateValues (which uses intersectDomains)
              let xDomain = domain xState
                  yDomain = domain yState
              in case updateValues xState yDomain >>= \updatedX ->
                      updateValues yState xDomain >>= \updatedY ->
                      Right (updatedX, updatedY) of
                   Right (updatedX, updatedY) ->
                     let changed = updatedX /= xState || updatedY /= yState
                         newVarStates = Map.insert xID updatedX $
                                        Map.insert yID updatedY varStates
                     in Right (changed, newVarStates)
                   Left errMsg -> Left errMsg -- intersection failed, contradiction
            _ -> Right (False, varStates)  -- one or both variables not initialized, no change
        _ -> Left $ "Variable name not found in nameToID map during x = y analysis: " ++ xName ++ " or " ++ yName

    -- EQUALITY Rule: x = e (where e is not just a variable)
    _ -> do
      xID <- lookupVarID xName nameToID
      xState <- lookupVarState xID varStates
      let omega = inferValues e nameToID varStates Nothing-- inferring domain of expression e
      -- updating x's state by intersecting its current domain with the inferred domain of e
      case updateValues xState omega of
        Right updatedState ->
          let changed = xState /= updatedState -- checking if the state actually changed
              updatedMap = if changed then Map.insert xID updatedState varStates else varStates
              (backwardChanged, finalStates) =
                  propagateExclusionsBackward e (domain updatedState) nameToID updatedMap
          in Right (changed || backwardChanged, finalStates)
        Left errMsg -> Left errMsg -- intersection failed, contradiction

-- ALREADY HANDLED BY THE ROOT RULE:
--analyzeConstraint (EqC cid (Var xName) (Int 0)) nameToID varStates =
--  markVarDefinitelyZero xName nameToID varStates

--analyzeConstraint (EqC cid (Int 0) (Var xName)) nameToID varStates =
--  markVarDefinitelyZero xName nameToID varStates  

-- ASSIGN Rule from PICUS paper: c * x = e  => x = e / c
analyzeConstraint (EqC _ (Mul (Int c) (Var xName)) e) nameToID varStates
  | let cModP = c `mod` p
  , cModP /= 0 = do -- ensuring c is not zero in the field
      xID <- lookupVarID xName nameToID
      xState <- lookupVarState xID varStates
      let omega = inferValues e nameToID varStates Nothing -- domain of e
      case modInverse cModP p of
        Nothing -> Left $ "Modular inverse does not exist for " ++ show cModP ++ " mod " ++ show p
        Just cInv -> do
          -- calculating the domain for x by multiplying omega by cInv
          let newDomainX = case omega of

                -- knownValues: Apply inverse directly
                KnownValues vSet ->
                  KnownValues (Set.map (\v -> (v * cInv) `mod` p) vSet)

                -- boundedValues: multiplying domain by cInv
                BoundedValues lbM ubM gapsSetE ->
                  case (lbM, ubM) of
                    (Just lbE, Just ubE) ->
                      -- checking if the original interval omega might contain zero.
                      if couldBeZero omega then
                        -- If omega might contain zero, multiplying by cInv can create
                        -- complex, non-contiguous sets.
                        -- Example: omega = [0, 2] = {0, 1, 2}. Let cInv = 12 (mod 17).
                        -- omega * cInv = {0*12, 1*12, 2*12} mod 17 = {0, 12, 24} mod 17 = {0, 12, 7}.
                        -- The set {0, 7, 12} is not a single interval and requires multiple
                        -- exclusion gaps (e.g., [0, 12] excluding (1, 6) and (8, 11))
                        -- to be represented by BoundedValues.
                        -- Calculating these precisely is complex, so we over-approximate
                        -- x's domain as unknown (`defaultValueDomain`) for simplicity and soundness.
                        -- TODO: Could potentially refine this case further.
                        defaultValueDomain
                      else
                        -- omega is guaranteed non-zero, multiplying bounds
                        let newLbX = (lbE * cInv) `mod` p
                            newUbX = (ubE * cInv) `mod` p
                            -- checking if the resulting interval [newLbX, newUbX] wraps around
                            wrapAround = newLbX > newUbX

                            -- mapping the exclusion intervals from omega (e) to x
                            -- each interval (lE, uE) becomes (lE*cInv, uE*cInv) mod p
                            mappedExclusionsX = Set.map (\(lE, uE) ->
                                      let lx = (lE * cInv) `mod` p
                                          ux = (uE * cInv) `mod` p
                                      -- TODO: the resulting interval (lx, ux) might wrap around itself
                                      in (lx, ux)
                                    ) gapsSetE

                            -- Combining exclusions:
                            -- 1. if the main interval [newLbX, newUbX] wrapped around,
                            --    we calculate its gap
                            -- 2. we add the mapped exclusions from the original domain
                            finalExclusionsSet = if wrapAround then
                                                    let wrapExclusionSet = getWrapAroundExclusion newUbX newLbX p
                                                    in Set.union wrapExclusionSet mappedExclusionsX
                                                  else
                                                    mappedExclusionsX

                            -- determining final bounds for x
                            (finalLbX, finalUbX) = if wrapAround then (0, p - 1) else (newLbX, newUbX)

                        in BoundedValues (Just finalLbX) (Just finalUbX) finalExclusionsSet

                    _ -> defaultValueDomain -- if omega has unknown bounds, x is unknown

          -- updating x's state using the calculated newDomainX
          updatedState <- updateValues xState newDomainX
          let changed = xState /= updatedState
              updatedMap = if changed then Map.insert xID updatedState varStates else varStates
          pure (changed, updatedMap)

  | otherwise = Right (False, varStates) -- if c is 0 mod p, constraint is 0 = e

-- handling symmetric e = c * x
analyzeConstraint (EqC cid e (Mul (Int c) (Var xName))) nameToID varStates =
    analyzeConstraint (EqC cid (Mul (Int c) (Var xName)) e) nameToID varStates

-- | ROOT Rule from PICUS paper: expr = 0
analyzeConstraint (EqC _ rootExpr (Int 0)) nameToID varStates =
    -- extracting root factors from the expression
    case extractRootFactors rootExpr of
        Just (xName, rootValues) -> do
            xID <- lookupVarID xName nameToID
            xState <- lookupVarState xID varStates
            let newDomain = KnownValues (Set.fromList rootValues)
            -- updating x's state
            updatedState <- updateValues xState newDomain
            let changed = xState /= updatedState
                updatedMap = if changed then Map.insert xID updatedState varStates else varStates
            pure (changed, updatedMap)
        Nothing -> Right (False, varStates)  -- not a ROOT pattern

-- Handling symmetric case: 0 = expr
analyzeConstraint (EqC cid (Int 0) rootExpr) nameToID varStates =
    analyzeConstraint (EqC cid rootExpr (Int 0)) nameToID varStates

-- | Rule 3 from Ecne
--    Sum-of-powers rule:  EqC cid (some expression) (Var zName)
--    If that expression is a sum of terms like c^k * b_k or b_k * c^k, with each b in [0,1],
--    then z ∈ [0, c^(maxExponent+1)-1].
analyzeConstraint (EqC cid lhs (Var zName)) nameToID varStates
  | Just terms <- checkSumOfPowers 2 lhs  -- TODO: generalize
  = do
      -- 1) we confirm each b_i is a Boolean variable
      let allBool = all (isBinaryVar nameToID varStates . fst) terms
      if not allBool
         then pure (False, varStates)
         else do
           -- 2) we infer z's upper bound = c^(1 + maxExp) - 1
           let exps    = map snd terms
               maxExp  = maximum exps
               cBase       = 2  -- TODO: generalize
               upBound = cBase^(maxExp + 1) - 1
           zID <- lookupVarID zName nameToID
           zState <- lookupVarState zID varStates

           -- 3) we update the variable 'z' with [0, upBound]
           updatedZState <-
             updateValues zState (BoundedValues (Just 0) (Just upBound) Set.empty)

           let changed1   = updatedZState /= zState
               varStates1 = Map.insert zID updatedZState varStates

           -- 4) If the analysis (potentially after intersecting with previous knowledge or other constraints)
           --    determines that 'z' must have a single, specific numerical value (i.e., its domain becomes
           --    KnownValues {v}), we can deduce the exact value of each boolean variable 'b_i'.
           --    Since z = b_0*c^0 + b_1*c^1 + ... + b_maxExp*c^maxExp, and each b_i is 0 or 1,
           --    the known value of 'z' essentially *is* the number represented in base 'c' by the bits 'b_i'.
           --    We can extract the required value (0 or 1) for each b_i by looking at the corresponding
           --    base-c digit of the known value of 'z'. This allows us to further constrain the domains
           --    of the 'b_i' variables, setting them to KnownValues {0} or KnownValues {1}.
           --    The `decodeSumOfPowers` function performs this bit extraction and state update.
           case domain updatedZState of
                 KnownValues zVals | Set.size zVals == 1 -> do
                   let knownZ = Set.findMin zVals -- getting the single value
                   varStates2 <- decodeSumOfPowers cBase knownZ terms nameToID varStates1
                   pure (True, varStates2)
                 _ -> pure (changed1, varStates1)

-- | Rule 4b from Ecne
--    "If 1 = x + y and x in [0,1], then y in [0,1], and vice versa"
analyzeConstraint (EqC cid (Int 1) (Add (Var xName) (Var yName))) nameToID varStates =
  zeroOne cid xName yName nameToID varStates

analyzeConstraint (EqC cid (Add (Var xName) (Var yName)) (Int 1)) nameToID varStates =
  zeroOne cid xName yName nameToID varStates

-- AND case
analyzeConstraint (AndC cid subCs) nameToID varStates = do
  let initialAcc = (False, varStates)  -- (changed?, current map)
  (finalChanged, finalMap) <- foldlAndM nameToID initialAcc subCs
  pure (finalChanged, finalMap)
  where
    -- we go over the sub-constraints in [Constraint] with a 
    -- left-biased Either: if any sub-constraint fails, we propagate the error.
    foldlAndM
      :: Map String Int
      -> (Bool, Map Int VariableState)
      -> [Constraint]
      -> Either String (Bool, Map Int VariableState)
    foldlAndM _ acc [] = Right acc
    foldlAndM nmToID (accChanged, accMap) (c:cs) = do
       (changedNow, newMap) <- analyzeConstraint c nmToID accMap
       let combinedChanged = accChanged || changedNow
       foldlAndM nmToID (combinedChanged, newMap) cs

-- TODO: handle the symmetrical case: EqC cid (Var zName) rhs
-- if checkSumOfPowers 2 rhs = ...

-- | NonZero rule.
-- Pattern 1: expr3 + expr1 * expr2 = c
-- Soundness: If we know expr3 is definitely 0 (mod p) and c is non-zero (mod p),
-- the constraint simplifies to expr1 * expr2 = c (mod p). In a field, if the product
-- of two elements (i.e., c) is non-zero, then neither element can be zero.
-- Therefore, we can conclude expr1 != 0 (mod p) and expr2 != 0 (mod p).
analyzeConstraint (EqC cid (Add expr3 (Mul expr1 expr2)) (Int c)) nameToID varStates = do
  let cModP = c `mod` p
  let domain3 = inferValues expr3 nameToID varStates Nothing
  if isCertainlyZeroDomain domain3 && cModP /= 0
      then markExprPairNonZero expr1 expr2 nameToID varStates
      else Right (False, varStates)

-- Pattern 2: expr1 * expr2 + c1 = c2
-- Soundness: This constraint is equivalent to expr1 * expr2 = c2 - c1 (mod p).
-- If c2 is not equal to c1 (mod p), then c2 - c1 is non-zero (mod p).
-- Let c' = c2 - c1. We have expr1 * expr2 = c' (mod p) where c' != 0 (mod p).
-- As in Pattern 1, this implies expr1 != 0 (mod p) and expr2 != 0 (mod p).
analyzeConstraint (EqC cid (Add (Mul expr1 expr2) (Int c1)) (Int c2)) nameToID varStates = do
  let c1ModP = c1 `mod` p
  let c2ModP = c2 `mod` p
  -- this corresponds to expr1 * expr2 = c2 - c1 (mod p)
  -- we need c2 - c1 != 0 (mod p), which means c2 != c1 (mod p)
  if c2ModP /= c1ModP
      then markExprPairNonZero expr1 expr2 nameToID varStates
      else Right (False, varStates)

-- Pattern 3: expr1 * expr2 = c
-- Soundness: The constraint is expr1 * expr2 = c (mod p).
-- If c is non-zero (mod p), then for the equality to hold in the field,
-- neither expr1 nor expr2 can be zero (mod p).
analyzeConstraint (EqC cid (Mul expr1 expr2) (Int c)) nameToID varStates = do
  let cModP = c `mod` p
  if cModP /= 0
      then markExprPairNonZero expr1 expr2 nameToID varStates
      else Right (False, varStates)

-- Symmetric cases for NonZero rules
analyzeConstraint (EqC cid (Int c) (Add expr3 (Mul expr1 expr2))) nameToID varStates =
  analyzeConstraint (EqC cid (Add expr3 (Mul expr1 expr2)) (Int c)) nameToID varStates

analyzeConstraint (EqC cid (Int c2) (Add (Mul expr1 expr2) (Int c1))) nameToID varStates =
  analyzeConstraint (EqC cid (Add (Mul expr1 expr2) (Int c1)) (Int c2)) nameToID varStates

analyzeConstraint (EqC cid (Int c) (Mul expr1 expr2)) nameToID varStates =
  analyzeConstraint (EqC cid (Mul expr1 expr2) (Int c)) nameToID varStates

analyzeConstraint _ _ varStates = Right (False, varStates)  -- TODO: handle other constraints


-- Helper function to constrain a variable to binary {0, 1}
constrainVarToBinary :: String -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)
constrainVarToBinary varName nameToID varStates = do
  varID <- lookupVarID varName nameToID
  oldState <- lookupVarState varID varStates
  -- using BoundedValues for consistency, could also use KnownValues {0, 1}
  let binaryDomain = BoundedValues (Just 0) (Just 1) Set.empty
  newState <- updateValues oldState binaryDomain
  let changed = newState /= oldState
  let newMap = if changed then Map.insert varID newState varStates else varStates
  pure (changed, newMap)

-- | Propagates exclusions backward from a result domain to variables in an expression.
propagateExclusionsBackward
    :: Expression
    -> ValueDomain  -- the domain of the variable the expression is equal to (e.g., x's domain)
    -> Map String Int
    -> Map Int VariableState
    -> (Bool, Map Int VariableState) -- returns (if any change occurred, updated states)
propagateExclusionsBackward expr xDomain nameToID varStates =
  let gapsX = getExclusionIntervals xDomain
  in if Set.null gapsX
     then (False, varStates)
     else go expr gapsX nameToID varStates
  where
    -- gets the list of excluded intervals, or Nothing if none
    getExclusionIntervals :: ValueDomain -> Set.Set (Integer, Integer)
    getExclusionIntervals (KnownValues _) = Set.empty -- KnownValues don't have separate exclusions
    getExclusionIntervals (BoundedValues _ _ currentGaps) = currentGaps

    -- helper to handle different expression structures
    go :: Expression -> Set.Set (Integer, Integer) -> Map String Int -> Map Int VariableState -> (Bool, Map Int VariableState)

    -- x = y - c  =>  y = x + c
    go (Sub (Var yName) (Int c)) intervalsX nameToID currentStates =
      -- for each interval (l, u) excluded for x, we calculate the set of excluded y values
      let excludedValuesY = concatMap (calculateExcludedSet (\v -> (v + c) `mod` p) p) (Set.toList intervalsX)
      in applyExclusionsToVar yName excludedValuesY nameToID currentStates

    -- x = c - y  =>  y = c - x
    go (Sub (Int c) (Var yName)) intervalsX nameToID currentStates =
      let excludedValuesY = concatMap (calculateExcludedSet (\v -> (c - v + p) `mod` p) p) (Set.toList intervalsX)
      in applyExclusionsToVar yName excludedValuesY nameToID currentStates

    -- x = y + c  =>  y = x - c
    go (Add (Var yName) (Int c)) intervalsX nameToID currentStates =
      let excludedValuesY = concatMap (calculateExcludedSet (\v -> (v - c + p) `mod` p) p) (Set.toList intervalsX)
      in applyExclusionsToVar yName excludedValuesY nameToID currentStates

    -- x = c + y => y = x - c (same as above)
    go (Add (Int c) (Var yName)) intervalsX nameToID currentStates =
      go (Add (Var yName) (Int c)) intervalsX nameToID currentStates

    -- x = y * c => y = x * c^-1
    go (Mul (Var yName) (Int c)) intervalsX nameToID currentStates =
      let cModP = c `mod` p
      in if cModP == 0 then
            -- if c is 0, x must be 0. If x excludes non-zero values, that's okay.
            -- If x excludes 0, it's a contradiction handled by updateValues earlier.
            (False, currentStates)
          else case modInverse cModP p of 
                Nothing -> (False, currentStates) -- cannot invert, cannot propagate
                Just cInv ->
                  let excludedValuesY = concatMap (calculateExcludedSet (\v -> (v * cInv) `mod` p) p) intervalsX
                  in applyExclusionsToVar yName excludedValuesY nameToID currentStates

    -- x = c * y (same as above)
    go (Mul (Int c) (Var yName)) intervalsX nameToID currentStates =
      go (Mul (Var yName) (Int c)) intervalsX nameToID currentStates

    -- cannot propagate exclusions backward for other complex expressions yet
    -- TODO
    go _ _ _ currentStates = (False, currentStates)


-- Helper: Given an operation `op`, modulus `p`, and an interval `(l, u)`,
-- calculates the set of values { op(v) mod p } for all v in the interval.
calculateExcludedSet :: (Integer -> Integer) -> Integer -> (Integer, Integer) -> [Integer]
calculateExcludedSet op p (l, u) =
    let valuesX = [l..u] -- TODO: check this, can be very inefficient if gaps are large
    in map (\v -> op v `mod` p) valuesX

-- Helper: Applies a list of exclusions to a specific variable using excludeValue repeatedly.
applyExclusionsToVar :: String -> [Integer] -> Map String Int -> Map Int VariableState -> (Bool, Map Int VariableState)
applyExclusionsToVar varName exclusions nameToID currentStates =
  case lookupVarID varName nameToID of
    Left _ -> (False, currentStates) -- var not found
    Right varID ->
      case lookupVarState varID currentStates of
        Left _ -> (False, currentStates) -- state not found
        Right oldVarState ->
          -- using foldl to apply each exclusion value
          foldl (\(changedAcc, statesAcc) valToExclude ->
                    -- getting the most recent state for the variable
                    let currentVarState = statesAcc Map.! varID
                        currentVarDomain = domain currentVarState
                        -- creating the domain with the single value excluded
                        domainWithExclusion = excludeValue currentVarDomain valToExclude
                    in case updateValues currentVarState domainWithExclusion of
                         -- if intersection fails (contradiction), we keep previous state 
                         -- TODO: should maybe log error?
                         Left _ -> (changedAcc, statesAcc)
                         Right newVarState ->
                           let changedNow = newVarState /= currentVarState
                           -- updating the state map only if a change occurred
                           in (changedAcc || changedNow, Map.insert varID newVarState statesAcc)
                ) (False, currentStates) exclusions -- initial state for fold

-- Marks an expression as non-zero and updates variable states accordingly.
-- Returns (changed, updatedStates) or an error message.
--
-- Why we don't just change bounds:
-- 1. Simply adjusting the lower/upper bounds of a BoundedValues domain is often
-- insufficient or incorrect when excluding a specific value 'c':
-- If the domain is [0, 10] and we need to exclude 5 (e.g., because x - 5 != 0),
-- the resulting set is {0, 1, 2, 3, 4} U {6, 7, 8, 9, 10}.
-- This split set cannot be represented by simply changing the bounds [0, 10].
-- We must introduce an exclusion.
-- 2. Wrap-around Intervals:
-- If the domain is a wrap-around interval like [15, 2] mod 17 (i.e., {15, 16, 0, 1, 2}),
-- excluding an internal value like 0 requires careful handling that simple bound
-- adjustments cannot manage generically.
markExprNonZero :: Expression -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)
markExprNonZero (Var xName) nameToID varStates = do
    xID <- lookupVarID xName nameToID
    oldXSt <- lookupVarState xID varStates
    let valToExclude = 0
    let currentDomain = domain oldXSt

    -- Step 1: Excluding 0 from the current domain.
    -- The `excludeValue` function handles both KnownValues (by set deletion)
    -- and BoundedValues (by adding (0, 0) to gaps and potentially
    -- adjusting bounds if they were exactly 0).
    let domainWithoutZero = excludeValue currentDomain valToExclude

    -- Step 2: Intersecting the original domain with the domain-without-zero using updateValues.
    -- We use `updateValues` (which calls `intersectDomains`) for several crucial reasons,
    -- rather than just using the result of `excludeValue` directly:
    --
    -- a) Consistency & Merging: `intersectDomains` correctly merges the "exclude zero"
    --    information with the variable's *existing* domain, including any pre-existing
    --    bounds and exclusion intervals. Simply using `domainWithoutZero` would discard
    --    prior information.
    --
    --
    -- b) Contradiction Detection: `intersectDomains` is essential for detecting if
    --    excluding zero leads to an invalid or empty state. Examples:
    --    - If the original domain was `KnownValues {0}`, the intersection results in an
    --      empty set, correctly identified by `intersectDomains`.
    --    - If the original domain was `BoundedValues [0, 0] Nothing`, `excludeValue` might
    --      produce an intermediate state with `gaps = Just [(0, 0)]`.
    --      `intersectDomains` correctly identifies this as an empty domain.
    --
    -- Therefore, using `updateValues` ensures the analysis remains sound.
    newState <- updateValues oldXSt domainWithoutZero
    let changed = newState /= oldXSt
    let newMap = if changed then Map.insert xID newState varStates else varStates
    pure (changed, newMap)

markExprNonZero (Sub (Int c) (Var xName)) nameToID varStates = do
    xID <- lookupVarID xName nameToID
    oldXSt <- lookupVarState xID varStates
    -- we need c - x != 0 (mod p), so x != c (mod p)
    let valToExclude = c `mod` p
    let currentDomain = domain oldXSt
    -- using excludeValue and updateValues for consistency and contradiction detection
    let domainWithoutC = excludeValue currentDomain valToExclude
    newState <- updateValues oldXSt domainWithoutC
    let changed = newState /= oldXSt
    let newMap = if changed then Map.insert xID newState varStates else varStates
    pure (changed, newMap)

markExprNonZero (Sub (Var xName) (Int c)) nameToID varStates = do
    -- x - c != 0 (mod p) => x != c (mod p)
    -- re-using the logic by calling the symmetric case handler
    markExprNonZero (Sub (Int c) (Var xName)) nameToID varStates

markExprNonZero (Int c) _ varStates =
    if c `mod` p == 0
    then Left $ "Contradiction: Constant expression " ++ show c ++ " is zero, cannot mark as non-zero."
    else Right (False, varStates) -- constant is non-zero, no state change needed

-- TODO: Add, Mul, etc. If e.g., Add e1 e2 != 0 and e1 is known zero,
-- then markExprNonZero e2 could be called I think??
markExprNonZero _ _ varStates = Right (False, varStates)

-- Marks a pair of expressions as non-zero.
markExprPairNonZero :: Expression -> Expression -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)
markExprPairNonZero expr1 expr2 nameToID varStates = do
    (changed1, states1) <- markExprNonZero expr1 nameToID varStates
    -- applying the second check to the potentially updated state from the first check
    (changed2, states2) <- markExprNonZero expr2 nameToID states1
    pure (changed1 || changed2, states2)

-- Extended Euclidean Algorithm: egcd a b = (g, x, y) where g = gcd(a, b) and ax + by = g
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0 = (a, 1, 0)
egcd a b =
    let (g, x1, y1) = egcd b (a `mod` b)
        x = y1
        y = x1 - (a `div` b) * y1
    in (g, x, y)

-- Modular Multiplicative Inverse: modInverse a m computes a^-1 mod m
-- Returns Nothing if the inverse does not exist (i.e., gcd(a, m) /= 1)
modInverse :: Integer -> Integer -> Maybe Integer
modInverse a m
 | m <= 1 = Nothing -- modulus must be > 1
 | otherwise =
    let (g, x, _) = egcd a m
    in if g /= 1
       then Nothing -- inverse doesn't exist
       else Just (x `mod` m) -- x might be negative, so we take mod m

-- Field modulus (e.g., BN254 prime)
-- TODO: We should obtain this from the IR and save this information in the Program structure.
fieldModulus :: Integer
fieldModulus = 21888242871839275222246405745257275088548364400416034343698204186575808495617

markNonZeroPair :: String -> String -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)
markNonZeroPair xName yName nameToID varStates = do
    xID <- lookupVarID xName nameToID
    yID <- lookupVarID yName nameToID
    oldXSt <- lookupVarState xID varStates
    oldYSt <- lookupVarState yID varStates

    -- trying to update x by excluding zero
    -- We use updateValues because excludeZero alone, while adding 0 to exclusions and
    -- adjusting bounds if they were exactly 0, doesn't perform the full consistency check.
    -- For instance, if oldXSt's domain was BoundedValues (Just 0) (Just 0) Nothing (i.e., known to be 0),
    -- excludeZero would produce BoundedValues (Just 1) (Just (-1)) (Just {0}) due to its bound adjustment logic.
    -- This intermediate state is inconsistent (lower bound > upper bound).
    -- The intersectDomains function (called by updateValues) is necessary to:
    -- 1. Correctly merge the "exclude zero" information with the variable's *existing* domain (bounds and exclusions).
    -- 2. Detect resulting inconsistencies like the lb > ub conflict shown above, or cases where exclusions empty the domain.
    -- This ensures the analysis remains sound.
    newXSt <- updateValues oldXSt (excludeZero (domain oldXSt))
    -- trying to update y by excluding zero
    newYSt <- updateValues oldYSt (excludeZero (domain oldYSt))

    let changed = newXSt /= oldXSt || newYSt /= oldYSt
        -- applying updates only if they succeeded
        newMap = Map.insert xID newXSt (Map.insert yID newYSt varStates)

    pure (changed, newMap)

-- | We try to convert `expr` into a sum of terms: c^exponent * Var(b) or Var(b) * c^exponent.
--   c^k * b_k + ... + c^m * b_m or b_k * c^k + ... + b_m * c^m
--   Returns Nothing if checking fails or if 'expr' is not that pattern.
checkSumOfPowers
  :: Integer -- c, the base of the powers
  -> Expression
  -> Maybe [(String, Integer)] -- list of (varName, exponent)
checkSumOfPowers c = go
  where
    go (Add l r) = do
      leftTerms  <- go l
      rightTerms <- go r
      pure (leftTerms ++ rightTerms)

    -- case 1: c^k * b
    go (Mul (Int k) (Var bName)) = do
      expK <- isExactPowerOf c k  -- e.g., if c=2, k=4 => exponent=2
      pure [(bName, expK)]

    -- case 2: b * c^k (symmetric case)
    go (Mul (Var bName) (Int k)) = do
      expK <- isExactPowerOf c k
      pure [(bName, expK)]

    -- c^0 * Var(b) is just (Var b), i.e. exponent=0
    go (Var bName) = Just [(bName, 0)]

    -- summation with 0 is a no-op: (Add e (Int 0)) => ignore the 0
    go (Int 0)     = Just []

    -- anything else => not recognized!
    go _           = Nothing

-- | if 'n' = c^exp exactly, we return that exponent; otherwise Nothing.
--   For c=2, e.g. (8 -> Just 3), (6 -> Nothing).
isExactPowerOf :: Integer -> Integer -> Maybe Integer
isExactPowerOf c n
  | n <= 0    = Nothing
  | otherwise = go 0 1
  where
    go e pow
      | pow == n  = Just e
      | pow >  n  = Nothing
      | otherwise = go (e+1) (pow*c)

-- | Tests if given var is binary (0 or 1).
isBinaryVar  :: Map String Int  -> Map Int VariableState -> String -> Bool
isBinaryVar nameToID varStates varName = 
  case lookupVarStateByName varName nameToID varStates of
    Left _ -> False -- Variable not found or no state
    Right st -> isIn01 st

-- | If the analysis (potentially after intersecting with previous knowledge or other constraints)
--    determines that 'z' must have a single, specific numerical value (i.e., its domain becomes
--    KnownValues {v}), we can deduce the exact value of each boolean variable 'b_i'.
--    Since z = b_0*c^0 + b_1*c^1 + ... + b_maxExp*c^maxExp, and each b_i is 0 or 1,
--    the known value of 'z' essentially *is* the number represented in base 'c' by the bits 'b_i'.
--    We can extract the required value (0 or 1) for each b_i by looking at the corresponding
--    base-c digit of the known value of 'z'. This allows us to further constrain the domains
--    of the 'b_i' variables, setting them to KnownValues {0} or KnownValues {1}.
--    The `decodeSumOfPowers` function performs this bit extraction and state update.
decodeSumOfPowers
  :: Integer                    -- c, the base
  -> Integer                    -- known value of z
  -> [(String, Integer)]        -- (bName, exponent) pairs
  -> Map String Int             -- nameToID
  -> Map Int VariableState      -- varStates
  -> Either String (Map Int VariableState)
decodeSumOfPowers cBase knownZ terms nameToID varStates0 =
  foldl step (Right varStates0) terms
  where
    step :: Either String (Map Int VariableState) -> (String, Integer) -> Either String (Map Int VariableState)
    step acc (bName, e) = do
      currentVarStates <- acc
      bID <- lookupVarID bName nameToID
      st <- lookupVarState bID currentVarStates

      let coeff   = cBase^e
          digit   = (knownZ `div` coeff) `mod` cBase  -- extracting the base-c digit
          -- we determine the required value for the bit variable
          requiredVal = if digit >= 1 then 1 else 0
          newDomain = KnownValues (Set.singleton requiredVal)

      -- updating the bit variable's state
      updatedSt <- updateValues st newDomain
      pure (Map.insert bID updatedSt currentVarStates)

-- | Helper function to invert the nameToID map
invertMap :: Map String Int -> Map Int String
invertMap nmToID =
  Map.fromList [ (vid, nm) | (nm, vid) <- Map.toList nmToID ]

-- | Transforms the final (Map Int VariableState) to a (Map String VariableState)
-- so that we have the variable names instead of IDs.
transformIDToNames
  :: Map String Int                   -- ^ nameToID
  -> Map Int VariableState            -- ^ final states keyed by Int
  -> Map String VariableState         -- ^ final states keyed by varName
transformIDToNames nmToID vStates =
  let idToName = invertMap nmToID
  in Map.fromList
      [ (idToName Map.! i, st) | (i, st) <- Map.toList vStates]

analyzeConstraints :: Map Int Constraint -> Map String Int -> Map Int [Int] -> Maybe [UserRule] -> Map Int VariableState -> Map String VariableState
analyzeConstraints constraints nameToID varToConstraints maybeRules = loop (initializeQueue (Map.elems constraints))
  where
    loop :: Seq Int -> Map Int VariableState -> Map String VariableState
    loop queue vStates =
      case viewl queue of
        Seq.EmptyL -> transformIDToNames nameToID vStates
        cId :< restQueue ->
          case Map.lookup cId constraints of
            Nothing -> loop restQueue vStates

            Just constraint ->
              case analyzeConstraint constraint nameToID vStates of
                ----------------------------------------------------
                -- CASE 1) built-in inference yields changes
                ----------------------------------------------------
                Right (True, newStates) ->
                  let affected = collectVarsFromConstraint nameToID constraint
                      newQ     = reQueue restQueue varToConstraints affected
                  in loop newQ newStates

                ----------------------------------------------------
                -- CASE 2) built-in inference yields no change
                ----------------------------------------------------
                Right (False, sameStates) -> 
                  case maybeRules of
                    -- no user rules => we move on
                    Nothing -> loop restQueue sameStates

                    -- user rules => we attempt them
                    Just userRs ->
                      let (userChanged, updatedStates) =
                            applyUserRules constraint userRs sameStates nameToID
                          in if userChanged
                               then 
                                 let affected = collectVarsFromConstraint nameToID constraint
                                     newQ = reQueue restQueue varToConstraints affected
                                 in loop newQ updatedStates
                               else
                                 loop restQueue updatedStates

                Left err -> error $ "Analysis error: " ++ err

-- Helper to re-queue constraints that reference changed variables
reQueue :: Seq Int -> Map Int [Int] -> [Int] -> Seq Int
reQueue oldQueue varToConstraints = foldl (\accQ varID ->
    let cIDs = Map.findWithDefault [] varID varToConstraints
    in foldl (|>) accQ cIDs
  ) oldQueue

--------------------------
-- 6) Main Analysis
--------------------------

-- given Program
analyzeProgram :: Program -> Map String VariableState
analyzeProgram (Program inputs compVars constrVars _ pfRecips retVars constraints) =
  let nameToID = buildVarNameToIDMap (inputs ++ compVars ++ constrVars)
      varStates = initializeVarStates (inputs ++ compVars ++ constrVars)
      varToConstraints = buildVarToConstraints nameToID constraints
      constraintMap = Map.fromList [(getConstraintID c, c) | c <- constraints]
  in analyzeConstraints constraintMap nameToID varToConstraints Nothing varStates

-- given File
analyzeFromFile :: FilePath -> IO ()
analyzeFromFile filePath = do
    content <- readFile filePath
    case parseAndCompile content of
        Left err -> putStrLn $ "Error: " ++ err  
        Right program -> do
            let store = analyzeProgram program
            putStrLn "\n====== Inferred Value Information ======\n"
            prettyPrintStore store

-- USER RULES -- TODO: move to separate file!

-- | Analyzer which also takes user rules into account.
analyzeFromFileWithRules
  :: FilePath           -- circuitFile
  -> Maybe FilePath     -- userRulesFile (Nothing if no rules)
  -> IO ()
analyzeFromFileWithRules circuitFile maybeRulesFile = do
  circuitSrc <- readFile circuitFile
  case parseAndCompile circuitSrc of
    Left err -> putStrLn $ "Error parsing circuit: " ++ err
    Right program -> do
      -- parsing user rules if present
      userRules <- case maybeRulesFile of
        Nothing -> pure Nothing
        Just f  -> do
          raw <- readFile f
          case parseUserRules raw of
            Left e   -> do
              putStrLn ("Error parsing user rules: " ++ e)
              pure Nothing
            Right rs -> pure (Just rs)

      let inferredStore = analyzeProgramWithRules program userRules

      putStrLn "\n===== Final Inferred Value Store ====="
      prettyPrintStore inferredStore

analyzeProgramWithRules :: Program -> Maybe [UserRule] -> Map String VariableState
analyzeProgramWithRules (Program inputs compVars constrVars _ pfRecips retVars constraints) maybeRules =
  let
      nameToID    = buildVarNameToIDMap (inputs ++ compVars ++ constrVars)
      varStates   = initializeVarStates (inputs ++ compVars ++ constrVars)
      constraintMap = Map.fromList [(getConstraintID c, c) | c <- constraints]
      varToCon    = buildVarToConstraints nameToID constraints

  in analyzeConstraints constraintMap nameToID varToCon maybeRules varStates

-- Suppose we matched the placeholders -> realVarNames.
-- Then we apply "x in {0,1}", meaning realVarName in {0,1}.
applyUserAction :: UserAction -> Map.Map String Int -> Map.Map Int VariableState
                -> (Bool, Map.Map Int VariableState)
applyUserAction (ConstrainSet placeholderName enumer) plHoNameToID varStates =
  case Map.lookup placeholderName plHoNameToID of
    Nothing -> (False, varStates)
    Just realID ->
      let oldState = Map.findWithDefault initVarStateDefault realID varStates
          newState = updateValues oldState (KnownValues enumer)
      in case newState of
                   Left msg         -> (False, varStates)
                   Right newState  -> (True, Map.insert realID newState varStates)

applyUserAction (ConstrainRange placeholderName lo up) plHoNameToID varStates =
  case Map.lookup placeholderName plHoNameToID of
    Nothing -> (False, varStates)
    Just realID ->
      let oldState = Map.findWithDefault initVarStateDefault realID varStates
          newState = updateValues oldState (BoundedValues (Just lo) (Just up) Set.empty)
        in case newState of
                Left msg         -> (False, varStates)
                Right newState  -> (True, Map.insert realID newState varStates)

applyUserRules :: Constraint -> [UserRule] -> Map.Map Int VariableState -> Map.Map String Int -> (Bool, Map.Map Int VariableState)
applyUserRules realC userRules varStates nameToID =
  foldl applySingleRule (False, varStates) userRules
  where
    applySingleRule :: (Bool, Map.Map Int VariableState) -> UserRule -> (Bool, Map.Map Int VariableState)
    applySingleRule (changed, vs) (UserRule pC acts) =
      case matchConstraint pC realC of
        Nothing -> (changed, vs)
        Just placeholderToRealName ->
          let 
              sub = Map.fromList -- converting "placeholder -> realVarName" to "placeholder -> realVarID"
                [ (ph, realID)
                | (ph, rvName) <- Map.toList placeholderToRealName
                , let realID = fromMaybe (-1) (Map.lookup rvName nameToID)
                ]
              (newChanged, newVS) =
                foldl
                  (\(ch, accVS) action ->
                    let (c, vs') = applyUserAction action sub accVS
                    in (ch || c, vs'))
                  (False, vs)
                  acts
          in (changed || newChanged, newVS)


--------------------------------------------------------------------------------
-- 7) Bug Detection
--------------------------------------------------------------------------------

-- Moved to separate file (BugDetection.hs)