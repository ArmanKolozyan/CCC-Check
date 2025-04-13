module ValueAnalysis.ValueDomain where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust) 

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
        let newLb = combineLowerBounds lb1 lb2
            newUb = combineUpperBounds ub1 ub2
            -- we combine exclusions by taking the union
            newExSet = combineExclusions ex1 ex2
            newEx = if Set.null newExSet then Nothing else Just newExSet

            -- we check for immediate bound conflict (e.g., new lower > new upper)
            boundConflict = case (newLb, newUb) of
                              (Just l, Just u) | l > u -> True
                              _ -> False
            -- we check if a resulting bound is now explicitly excluded
            boundExclusionConflict = (maybe False (`Set.member` newExSet) newLb) ||
                                     (maybe False (`Set.member` newExSet) newUb)
        in if boundConflict then Left $ "Bound conflict during intersection: new lower " ++ show newLb ++ " > new upper " ++ show newUb
           else if boundExclusionConflict then Left $ "Bound excluded during intersection: bounds " ++ show (newLb, newUb) ++ ", exclusions " ++ show newExSet
           -- we check if the resulting range [lb, ub] combined with exclusions becomes empty (e.g., [5, 5] with excluded {5})
           else if case (newLb, newUb) of
                      (Just l, Just u) | l == u && maybe False (Set.member l) newEx -> True
                      _ -> False
                then Left $ "Intersection resulted in empty domain: bounds " ++ show (newLb, newUb) ++ " with exclusions " ++ show newExSet
           else Right $ BoundedValues newLb newUb newEx

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