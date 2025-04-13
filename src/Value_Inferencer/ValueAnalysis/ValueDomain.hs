module ValueAnalysis.ValueDomain where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust, isNothing) 

-- | Represents values of variables.
data ValueDomain
  = KnownValues (Set Integer) -- explicitly known values
  | BoundedValues (Maybe Integer) (Maybe Integer) (Maybe (Set Integer))   -- lower bound, upper bound, excluded values
  deriving (Eq, Show)

-- | Default value domain (completely unknown).
defaultValueDomain :: ValueDomain
defaultValueDomain = BoundedValues Nothing Nothing Nothing

-- | Checks if a value domain guarantees the value is non-zero.
isDefinitelyNonZero :: ValueDomain -> Bool
isDefinitelyNonZero (KnownValues s) = not (Set.null s) && not (Set.member 0 s)
isDefinitelyNonZero (BoundedValues lb ub excluded) =
    let zeroExcluded = maybe False (Set.member 0) excluded
        boundsExcludeZero = isJust lb && lb > Just 0
    in zeroExcluded || boundsExcludeZero

-- | Intersects two value domains. This is the core logic for value updates.
-- Returns Left with an error message if the intersection results in a contradiction (empty domain).
-- NOTE: Having a bounded domain value with exclusions that fall within the bounds is not an issue!
--       It is just a restriction of the possible values that can lie within the bounds.
--       The only issue is if the exclusions remove all possible values.
--       This is also taken into account by this function.  
intersectDomains :: ValueDomain -> ValueDomain -> Either String ValueDomain
intersectDomains d1 d2 = case (d1, d2) of
    -- both Known: we ntersect the sets
    (KnownValues s1, KnownValues s2) ->
        let intersection = Set.intersection s1 s2
        in if Set.null intersection
           then Left $ "Intersection of KnownValues resulted in empty set: " ++ show s1 ++ " and " ++ show s2
           else Right $ KnownValues intersection

    -- known and bounded: we filter known set by bounds and exclusions
    (KnownValues s, BoundedValues lb ub ex) -> filterKnown s lb ub ex
    (BoundedValues lb ub ex, KnownValues s) -> filterKnown s lb ub ex

    -- both Bounded: we combine bounds and exclusions
    (BoundedValues lb1 ub1 ex1, BoundedValues lb2 ub2 ex2) ->
        let combinedLb = combineLowerBounds lb1 lb2
            combinedUb = combineUpperBounds ub1 ub2

            -- we combine exclusions by taking the union
            newExSet = combineExclusions ex1 ex2
            newEx = if Set.null newExSet then Nothing else Just newExSet

            -- we adjust bounds iteratively based on exclusions
            adjustedLb = findNextValidLowerBound combinedLb newExSet
            adjustedUb = findPrevValidUpperBound combinedUb newExSet

            -- we check for immediate bound conflict (e.g., new lower > new upper)
            boundConflict = case (adjustedLb, adjustedUb) of
                              (Just l, Just u) | l > u -> True
                              _ -> False
            
            -- we heck for conflict where exclusions removed all values
            -- (This happens if adjusted bounds are both Nothing,
            -- but at least one initial bound was Just)
            adjustedBoundConflict_ExclusionEmptied =
              isNothing adjustedLb && isNothing adjustedUb && (isJust combinedLb || isJust combinedLb)                  
        
        in if boundConflict 
          then Left $ "Bound conflict during intersection: new lower " ++ show adjustedLb ++ " > new upper " ++ show adjustedUb
          else if adjustedBoundConflict_ExclusionEmptied then
                  Left $ "Intersection resulted in empty domain (exclusions removed all values): initial bounds were ["
                       ++ show combinedLb ++ ", " ++ show combinedUb ++ "], exclusions " ++ show newExSet
          else Right $ BoundedValues adjustedLb adjustedUb newEx

-- Helper to filter a KnownValues set by Bounded constraints
filterKnown :: Set Integer -> Maybe Integer -> Maybe Integer -> Maybe (Set Integer) -> Either String ValueDomain
filterKnown s lb ub maybeEx =
    let excludedSet = fromMaybe Set.empty maybeEx
        -- we filter by bounds first
        boundedS = case (lb, ub) of
                     (Just l, Just u) -> Set.filter (\x -> x >= l && x <= u) s
                     (Just l, Nothing) -> Set.filter (>= l) s
                     (Nothing, Just u) -> Set.filter (<= u) s
                     (Nothing, Nothing) -> s
        -- we then filter by exclusions
        finalS = Set.difference boundedS excludedSet

    in if Set.null finalS && not (Set.null s) -- checking if filtering made a non-empty set empty
       then Left $ "Filtering KnownValues " ++ show s ++ " by bounds " ++ show (lb, ub) ++ " and exclusions " ++ show excludedSet ++ " resulted in empty set."
       else Right $ KnownValues finalS

-- Helper to combine lower bounds (takes max)
combineLowerBounds :: Maybe Integer -> Maybe Integer -> Maybe Integer
combineLowerBounds (Just a) (Just b) = Just (max a b)
combineLowerBounds a Nothing = a
combineLowerBounds Nothing b = b

-- Helper to combine upper bounds (takes min)
combineUpperBounds :: Maybe Integer -> Maybe Integer -> Maybe Integer
combineUpperBounds (Just a) (Just b) = Just (min a b)
combineUpperBounds a Nothing = a
combineUpperBounds Nothing b = b

-- Helper to combine exclusion sets (takes union)
combineExclusions :: Maybe (Set Integer) -> Maybe (Set Integer) -> Set Integer
combineExclusions maybeS1 maybeS2 = Set.union (fromMaybe Set.empty maybeS1) (fromMaybe Set.empty maybeS2)


-- Helper: Finds the smallest integer >= l that is not in the exclusion set.
-- Returns Nothing if all integers >= l are excluded (or if l itself is Nothing).
findNextValidLowerBound :: Maybe Integer -> Set Integer -> Maybe Integer
findNextValidLowerBound Nothing _ = Nothing
findNextValidLowerBound (Just l) excludedSet
  | Set.member l excludedSet = findNextValidLowerBound (Just (l + 1)) excludedSet
  | otherwise = Just l

-- Helper: Finds the largest integer <= u that is not in the exclusion set.
-- Returns Nothing if all integers <= u are excluded (or if u itself is Nothing).
findPrevValidUpperBound :: Maybe Integer -> Set Integer -> Maybe Integer
findPrevValidUpperBound Nothing _ = Nothing
findPrevValidUpperBound (Just u) excludedSet
  | Set.member u excludedSet = findPrevValidUpperBound (Just (u - 1)) excludedSet
  | otherwise = Just u