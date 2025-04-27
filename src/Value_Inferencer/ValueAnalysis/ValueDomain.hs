module ValueAnalysis.ValueDomain where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Map
import qualified Data.Map.Merge.Strict as Map
import Syntax.AST

-- TODO: we maken vaak onderscheid tussen normal interval en wrapped,
-- maar eigenlijk kan wrapped nooit voorkomen want wordt omgezet naar
-- normal + exclusions (i.e., gaps).

-- TODO: terminologie "exclusions" en "gaps" consistent maken

-- | Represents values of variables.
-- `gaps` stores intervals (l, u) of values *excluded* from the domain.
-- This allows representing non-contiguous sets.
-- Example (p=10): BoundedValues {lb=Just 0, ub=Just 9, gaps=Set.singleton (3, 5)}
-- represents the set {0, 1, 2, 6, 7, 8, 9}.
-- A wrap-around interval like [8, 1] (mod 10) is represented by its gap:
-- BoundedValues {lb=Just 0, ub=Just 9, gaps=Set.singleton (2, 7)}.
-- TODO: check of we niet gewoon alles via BoundedValues kunnen doen (zonder veel efficiëntie te verliezen).
data ValueDomain
  = KnownValues (Set Integer) -- explicitly known values
  | BoundedValues
    { lowerBound :: Maybe Integer,
    upperBound :: Maybe Integer,
    -- Intervals [l, u] that are excluded.
    -- For a wrap-around [a, p-1] U [0, b], the gap is [(b+1, a-1)].
    gaps :: Set (Integer, Integer)
    }
  | ArrayDomain
    { -- Domains for specific indices.
      elementDomains :: Map Integer ValueDomain, 
      -- The inferred domain for any array element whose index 
      -- isn't explicitly tracked in the elementDomains map. 
      -- While it starts as the most general domain, future analysis 
      -- steps or specific constraints (e.g., a constraint applying 
      -- to all elements) could potentially refine this defaultElementDomain 
      -- to something more specific than defaultValueDomain. 
      -- Keeping it separate allows for this possibility.
      defaultElementDomain :: ValueDomain,      -- domain for unspecified indices
      -- Having arraySize directly within ArrayDomain is for convenience
      -- during the analysis phase. Functions operating on ArrayDomain
      -- can access the size directly from the domain object without 
      -- needing to refer back to the original variable's Binding and Sort.
      arraySize :: Integer                     -- size of the array
    }  
  deriving (Eq, Show)

-- | Determines the set of possible concrete integer indices within array bounds.
getPossibleIndices :: ValueDomain -> Integer -> Set Integer
getPossibleIndices idxDom size =
  let validRange = Set.fromList [0 .. size - 1] -- set of valid indices [0, size-1]
  in Set.intersection validRange $ -- intersecting possible indices with valid range
       case idxDom of
         KnownValues s -> s -- if indices are known, we use them directly
         BoundedValues lbM ubM gaps ->
           let -- determining the initial range based on bounds, clamped by 0 and size-1
               minIdx = max 0 (fromMaybe 0 lbM)
               maxIdx = min (size - 1) (fromMaybe (size - 1) ubM)
               initialIndices = Set.fromList [minIdx .. maxIdx]

               -- function to check if an index falls within any gap
               isInGap idx = any (\(gapL, gapU) -> idx >= gapL && idx <= gapU) gaps

               -- filtering out indices that are within gaps
           in Set.filter (not . isInGap) initialIndices

         -- array as index? Invalid. Returning empty set.
         ArrayDomain {} -> Set.empty  

-- Helper function for merging element maps during intersection
mergeElemMaps :: Map Integer ValueDomain -> ValueDomain -> Map Integer ValueDomain -> ValueDomain -> Either String (Map Integer ValueDomain)
mergeElemMaps elems1 def1 elems2 def2 =
  Map.mergeA
    (Map.traverseMissing (\_k d1_i -> intersectDomains d1_i def2)) -- key only in elems1: intersecting d1_i with def2
    (Map.traverseMissing (\_k d2_i -> intersectDomains def1 d2_i)) -- key only in elems2: intersect def1 with d2_i
    (Map.zipWithAMatched (\_k d1_i d2_i -> intersectDomains d1_i d2_i)) -- key in both: intersecting d1_i with d2_i
    elems1
    elems2

-- Default modulus for the field (BN254).
-- TODO: compile this from the program code
p :: Integer
p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

-- | Default value domain (can be every possible value).
defaultValueDomain :: ValueDomain
defaultValueDomain = BoundedValues (Just 0) (Just (p - 1)) Set.empty

-- | Checks if a value domain guarantees the value is non-zero.
-- For this to be the case, zero must be explicitly excluded via gaps 
-- OR the bounds must strictly exclude zero.
isDefinitelyNonZero :: ValueDomain -> Bool
isDefinitelyNonZero (KnownValues s) = not (Set.null s) && not (Set.member 0 s)
isDefinitelyNonZero (BoundedValues lbM ubM currentGaps) =
    let zeroIsExcluded = isExcluded 0 currentGaps p
        boundsStrictlyExcludeZero = case (lbM, ubM) of
                                      (Just lb, Just ub) ->
                                          (lb <= ub) && (lb > 0)
                                          -- ^ otherwise we have a wrapped interval [lb, p-1] U [0, ub] 
                                          -- and wrapped intervals always contain 0 unless excluded

                                      (Just lb, Nothing) -> lb > 0 -- lower bound > 0
                                      _ -> False
    in zeroIsExcluded || boundsStrictlyExcludeZero


-- Helper function to check if a domain might possibly contain zero.
couldBeZero :: ValueDomain -> Bool
couldBeZero (KnownValues s) = Set.member 0 s
couldBeZero (BoundedValues lbM ubM currentGaps) =
    let zeroIsExcluded = isExcluded 0 currentGaps p
        -- checking if 0 is within the bounds (or if bounds are unknown)
        zeroInRange = case (lbM, ubM) of
                        (Just lb, Just ub) ->
                            if lb <= ub then -- standard interval [lb, ub]
                                lb <= 0 && ub >= 0
                            else -- wrapped interval [lb, p-1] U [0, ub]
                                True -- wrapped intervals always contain 0 unless excluded
                        _ -> True -- unknown bounds, we assume 0 is possible
    in not zeroIsExcluded && zeroInRange

-- Helper function to check if a domain is certainly the single value zero.
isCertainlyZeroDomain :: ValueDomain -> Bool
isCertainlyZeroDomain (KnownValues s) = s == Set.singleton 0
isCertainlyZeroDomain (BoundedValues lbM ubM currentGaps) =
    let -- bounds must be exactly [0, 0]
        boundsAreZero = case (lbM, ubM) of
                          (Just 0, Just 0) -> True
                          _                -> False
        -- and 0 must NOT be excluded
        zeroIsExcluded = isExcluded 0 currentGaps p
    in boundsAreZero && not zeroIsExcluded

-- | Intersects two value domains.
-- Returns Left with an error message if the intersection results in a contradiction (empty domain).
-- NOTE: Having a bounded domain value with exclusions that fall within the bounds is not an issue!
--       It is just a restriction of the possible values that can lie within the bounds.
--       The only issue is if the exclusions remove all possible values.
--       This is also taken into account by this function.  
intersectDomains :: ValueDomain -> ValueDomain -> Either String ValueDomain
intersectDomains d1 d2 = case (d1, d2) of

    -- Case 1: KnownValues intersection
    (KnownValues s1, KnownValues s2) ->
        let intersection = Set.intersection s1 s2
        in if Set.null intersection
           then Left $ "Intersection of KnownValues resulted in empty set: " ++ show s1 ++ " and " ++ show s2
           else Right $ KnownValues intersection

    -- Case 2: KnownValues intersected with BoundedValues
    -- (knownvalues are always small sets, so we enumerate them instead of the bounded one)
    (KnownValues s, BoundedValues lb ub gaps) -> filterKnownByBounded s lb ub gaps p
    (BoundedValues lb ub gaps, KnownValues s) -> filterKnownByBounded s lb ub gaps p

    -- Case 3: BoundedValues intersection
    (BoundedValues lb1 ub1 gaps1, BoundedValues lb2 ub2 gaps2) ->
        -- combining bounds: max of lower, min of upper (to over-approximate)
        let combinedLb = combineBounds max lb1 lb2
            combinedUb = combineBounds min ub1 ub2

        -- checking for immediate bound conflicts (before considering wrap-around or exclusions)
        in case (combinedLb, combinedUb) of
             (Just l, Just u) | l > u ->
                Left $ "Initial bound conflict during intersection: new lower " ++ show l ++ " > new upper " ++ show u
             _ -> do
                 -- combining gaps using Set.union
                 let combinedGaps = Set.union gaps1 gaps2

                 -- removing wrong intervals covering the whole field
                 -- TODO: beter noteren, ging om intervals waar (l, l - 1) had
                 let correctGaps = Set.filter (\(l, u) -> (l - 1 + p) `mod` p /= u) combinedGaps

                 -- finding the actual lower bound >= combinedLb, avoiding exclusions
                 -- TODO: dit is volgens mij toch niet strictly nodig, + 
                 -- als we de bounds aanpassen, dan mogen we denk ik correctGaps weggooien?
                 -- HMMM JAWEL? VOOR EMPTINESS DETECTION
                 finalLb <- findNextValidLowerBoundInterval combinedLb correctGaps p

                 -- finding the actual upper bound <= combinedUb, avoiding exclusions
                 finalUb <- findPrevValidUpperBoundInterval combinedUb correctGaps p

                 if finalLb > finalUb then
                     Left $ "Intersection resulted in empty domain: adjusted lower bound " ++ show finalLb ++ " > adjusted upper bound " ++ show finalUb ++
                            ". Initial bounds: [" ++ show combinedLb ++ ", " ++ show combinedUb ++ "], Final Exclusions: " ++ show correctGaps
                 else
                     Right $ BoundedValues (Just finalLb) (Just finalUb) correctGaps
    
    -- Case 4: ArrayDomain intersection
    (ArrayDomain elems1 def1 size1, ArrayDomain elems2 def2 size2) ->
      if size1 /= size2
      then Left "Array size mismatch during intersection" -- sizes must match
      else do
        -- intersecting default domains
        intersectedDef <- intersectDomains def1 def2

        -- intersecting element domains using Map.merge
        intersectedElems <- mergeElemMaps elems1 def1 elems2 def2

        Right (ArrayDomain intersectedElems intersectedDef size1)

    -- intersecting Array with Scalar: contradiction
    (ArrayDomain {}, _) -> Left "Cannot intersect ArrayDomain with non-ArrayDomain"
    (_, ArrayDomain {}) -> Left "Cannot intersect non-ArrayDomain with ArrayDomain"

-- Helper to combine Maybe bounds using a function (max for lower bounds, min for upper bounds)
combineBounds :: (Integer -> Integer -> Integer) -> Maybe Integer -> Maybe Integer -> Maybe Integer
combineBounds _ Nothing Nothing = Nothing
combineBounds _ (Just a) Nothing = Just a
combineBounds _ Nothing (Just b) = Just b
combineBounds f (Just a) (Just b) = Just (f a b)

-- Helper to combine exclusion sets (takes union)
combineExclusions :: Maybe (Set Integer) -> Maybe (Set Integer) -> Set Integer
combineExclusions maybeS1 maybeS2 = Set.union (fromMaybe Set.empty maybeS1) (fromMaybe Set.empty maybeS2)

-- Helper to filter a KnownValues set by Bounded constraints (bounds and interval exclusions)
-- Since knownvalues are always small sets, we enumerate them instead of the bounded one!
filterKnownByBounded :: Set Integer -> Maybe Integer -> Maybe Integer -> Set (Integer, Integer) -> Integer -> Either String ValueDomain
filterKnownByBounded s lbM ubM currentGaps p =
    -- filtering by bounds first
    let boundedS = case (lbM, ubM) of
                     (Just lb, Just ub) ->
                         if lb <= ub then Set.filter (\x -> x >= lb && x <= ub) s -- standard interval
                         else Set.filter (\x -> x >= lb || x <= ub) s            -- wrapped interval
                     (Just lb, Nothing) -> Set.filter (>= lb) s -- lower bound only
                     (Nothing, Just ub) -> Set.filter (<= ub) s -- upper bound only
                     (Nothing, Nothing) -> s                   -- no bounds

        -- then filtering by exclusion intervals
        finalS = Set.filter (\x -> not (isExcluded x currentGaps p)) boundedS

    -- checking if filtering made a non-empty set empty
    in if Set.null finalS && not (Set.null s) 
       then Left $ "Filtering KnownValues " ++ show s ++ " by bounds " ++ show (lbM, ubM) ++ " and exclusions " ++ show currentGaps ++ " resulted in empty set."
       else Right $ KnownValues finalS

-- Helper: Finds the smallest integer >= initialLb (if Just) that is not excluded.
-- Returns Left if all values >= initialLb are excluded.
-- TODO: check, dit kan lang duren als lb groot is!    
findNextValidLowerBoundInterval :: Maybe Integer -> Set (Integer, Integer) -> Integer -> Either String Integer
findNextValidLowerBoundInterval Nothing _ _ = Right 0 -- if initial lower bound unknown, we start search from 0
findNextValidLowerBoundInterval (Just lb) currentGaps p =
    findNext lb
    where
      findNext currentLb
        | isExcluded currentLb currentGaps p =
            let nextVal = (currentLb + 1) `mod` p
            -- stop condition: if we wrap around back to the original lb, everything is excluded
            in if nextVal == lb then
                 Left $ "Could not find valid lower bound >= " ++ show lb ++ "; all values excluded."
               else
                 findNext nextVal
        | otherwise = Right currentLb

-- Helper: Finds the largest integer <= initialUb (if Just) that is not excluded.
-- Returns Left if all values <= initialUb are excluded.
-- TODO: check, dit kan lang duren als ub klein is!    
findPrevValidUpperBoundInterval :: Maybe Integer -> Set (Integer, Integer) -> Integer -> Either String Integer
findPrevValidUpperBoundInterval Nothing _ p = Right (p - 1) -- if initial upper bound unknown, we start search from p-1
findPrevValidUpperBoundInterval (Just ub) currentGaps p =
    findPrev ub
    where
      findPrev currentUb
        | isExcluded currentUb currentGaps p =
            let prevVal = (currentUb - 1 + p) `mod` p
            -- stop condition: If we wrap around back to the original ub, everything is excluded
            in if prevVal == ub then
                 Left $ "Could not find valid upper bound <= " ++ show ub ++ "; all values excluded."
               else
                findPrev prevVal
        | otherwise = Right currentUb

-- | Excludes a specific value from a domain.
excludeValue :: ValueDomain -> Integer -> ValueDomain
excludeValue (KnownValues s) v = KnownValues (Set.delete v s)
excludeValue d@(BoundedValues lbM ubM currentGaps) v =
  let vModP = v `mod` p
      -- checking if v is already outside the bounds (simplified check)
      outsideBounds = case (lbM, ubM) of
                        (Just lb, Just ub) -> if lb <= ub then vModP < lb || vModP > ub else vModP > ub && vModP < lb
                        _ -> False
      -- checking if v is already excluded
      alreadyExcluded = isExcluded vModP currentGaps p

  in if outsideBounds || alreadyExcluded then
       d
     else
       -- inserting the new interval (v, v) as gap
       let newInterval = (vModP, vModP)
           updatedIntervals = Set.insert newInterval currentGaps
       in BoundedValues lbM ubM updatedIntervals

-- | Checks if a value `v` falls within any of the excluded intervals.
isExcluded :: Integer -> Set (Integer, Integer) -> Integer -> Bool
isExcluded v intervals p = any checkInterval (Set.toList intervals)
  where
    checkInterval (l, u)
      | l <= u    = v >= l && v <= u -- standard interval
      | otherwise = v >= l || v <= u -- wrapped interval

-- | Excludes zero from a domain.
excludeZero :: ValueDomain -> ValueDomain
excludeZero domain = excludeValue domain 0

-- Checks if a domain is unconstrained.
-- TODO: check nog eens of de definitie klopt in geval van sets
domainIsUnconstrained :: ValueDomain -> Bool
domainIsUnconstrained (KnownValues s) = Set.null s
domainIsUnconstrained (BoundedValues (Just 0) (Just ul) _)
  | ul == p - 1 = True
domainIsUnconstrained _ = False

-- | Converts a Tag into its corresponding ValueDomain constraint.
tagToDomain :: Tag -> ValueDomain
tagToDomain (SimpleTag "binary")  = BoundedValues (Just 0) (Just 1) Set.empty
tagToDomain (SimpleTag "nonzero") = excludeValue defaultValueDomain 0
tagToDomain (MaxBitsTag n)        = BoundedValues (Just 0) (Just ((2 ^ n) - 1)) Set.empty
tagToDomain (MaxValTag n)         = BoundedValues (Just 0) (Just n) Set.empty
tagToDomain _                     = defaultValueDomain -- default for unhandled tags