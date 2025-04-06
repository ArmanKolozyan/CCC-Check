{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module ValueAnalysis.Analysis (analyzeProgram, VariableState(..), detectBugs) where

import Syntax.AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, (|>), viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)

--------------------------
-- 1) Variable State Representation
--------------------------

-- | Tracks the state of each variable.
data VariableState = VariableState
  {
    values :: Set Integer,
    low_b :: Maybe Integer,
    upp_b :: Maybe Integer,
    nonZero :: Bool
  }
  deriving (Eq, Show)

--------------------------
-- 2) Initializing Variable States
--------------------------

-- | Initializing VariableState (no restrictions at the beginning).
initVarState :: VariableState
initVarState = VariableState
  { values = Set.empty,
    low_b = Nothing,
    upp_b = Nothing,
    nonZero = False
  }

-- | Builds a map from variable IDs to their initial state.
initializeVarStates :: [Binding] -> Map Int VariableState
initializeVarStates vars = Map.fromList [(vid v, initVarState) | v <- vars]

-- | Builds a map from variable names to their IDs for lookup.
-- TODO: just replace all vars in constraints by their ID during compilation!
buildVarNameToIDMap :: [Binding] -> Map String Int
buildVarNameToIDMap vars = Map.fromList [(name v, vid v) | v <- vars]

setNonZero :: VariableState -> VariableState
setNonZero vs = vs { nonZero = True
                   -- we also remove 0 from values if it exists
                   , values = Set.delete 0 (values vs)
                   }

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
updateValues :: VariableState -> ValueDomain -> Either String VariableState
updateValues vState (KnownValues newVals)
  -- if we already had some explicit values, we check consistency
  | not (Set.null (values vState)) =
      let oldVals      = values vState
          oldNonZero   = nonZero vState
          includesZero = Set.member 0 newVals
      in
        if not (Set.isSubsetOf newVals oldVals)
           then Left $ "Inconsistent value inference! New values " ++ show newVals
                    ++ " are not a subset of existing " ++ show oldVals
           
           -- we also check if the old nonZero was True but newVals includes 0:
           else if oldNonZero && includesZero
               then Left $ "Contradiction: var was NonZero but new values " 
                        ++ show newVals ++ " contain 0!"
               
               else 
                 -- safe update
                 let updatedNonZero = not includesZero
                 in Right vState 
                    { values  = newVals
                    , nonZero = updatedNonZero
                    }
  
  -- if no explicit values, we infer from bounds or just assign directly
  | otherwise =
      let lowerBound   = fromMaybe (minimum newVals) (low_b vState)
          upperBound   = fromMaybe (maximum newVals) (upp_b vState)
          inferredVals = Set.fromList [lowerBound .. upperBound]
          includesZero = Set.member 0 newVals
          oldNonZero   = nonZero vState
      in
        if not (Set.isSubsetOf newVals inferredVals)
           then Left $ "Value inference contradicts bounds! New values: "
                    ++ show newVals ++ " do not fit in range: "
                    ++ show [lowerBound .. upperBound]
        else if oldNonZero && includesZero
           then Left $ "Contradiction: var was NonZero but new values " 
                    ++ show newVals ++ " contain 0!"
        else 
          let updatedNonZero = not includesZero
          in Right vState 
             { values  = newVals
             , nonZero = updatedNonZero
             }

updateValues vState (BoundedValues maybeLb maybeUb) =
  -- we have bounding info, not an exact set.
  -- So we unify it with any existing explicit values or bounds.
  let oldVals      = values vState
      oldLb        = low_b vState
      oldUb        = upp_b vState
      oldNonZero   = nonZero vState
      newLb        = case (oldLb, maybeLb) of
                       (Just a, Just b) -> Just (max a b)
                       (Just a, Nothing) -> Just a
                       (Nothing, Just b) -> Just b
                       (Nothing, Nothing) -> Nothing
      newUb        = case (oldUb, maybeUb) of
                       (Just a, Just b) -> Just (min a b)
                       (Just a, Nothing) -> Just a
                       (Nothing, Just b) -> Just b
                       (Nothing, Nothing) -> Nothing

  in case (newLb, newUb) of
       (Just lb, Just ub) | lb > ub ->
          Left $ "Bound conflict: lower bound " ++ show lb
               ++ " exceeds upper bound " ++ show ub
       
       -- if we already had explicit values, we need to keep only those that fit
       -- in the new (lb..ub)
       _ ->
        let restrictedVals = 
              case (Set.null (values vState), newLb, newUb) of
                (True,  _, _ )       -> Set.empty
                (False, Just lb, Just ub) ->
                    Set.filter (\x -> x >= lb && x <= ub) (values vState)
                (False, Just lb, Nothing) ->
                    Set.filter (\x -> x >= lb) (values vState)
                (False, Nothing, Just ub) ->
                    Set.filter (\x -> x <= ub) (values vState)
                (False, Nothing, Nothing) ->
                    values vState

            includesZero = Set.member 0 restrictedVals
            oldNonZero   = nonZero vState

            -- if oldNonZero is true but we see a 0, that's a contradiction
            newNonZero =
              if oldNonZero && includesZero
                then error $ "Contradiction: var was NonZero but domain includes 0!"
                else oldNonZero && not includesZero

        in
          -- updating the state
          Right vState
            { values  = restrictedVals
            , low_b   = newLb
            , upp_b   = newUb
            , nonZero = newNonZero
            }
getVarID = Map.lookup

data ValueDomain
  = KnownValues (Set Integer)   -- explicitly known values
  | BoundedValues (Maybe Integer) (Maybe Integer)  -- (lower bound, upper bound)
  deriving (Eq, Show)

expandBounds :: Maybe Integer -> Maybe Integer -> [Integer]
expandBounds (Just lb) (Just ub) = [lb .. ub]  -- expands into a list
expandBounds _ _ = []  -- no meaningful bounds

-- | Recursively infers possible values of an expression
-- TODO: fix code duplication
inferValues :: Expression -> Map String Int -> Map Int VariableState -> ValueDomain
inferValues (Int c) _ _ = KnownValues (Set.singleton c)

inferValues (Var xName) nameToID varStates =
  case getVarID xName nameToID of
    Just varID -> case Map.lookup varID varStates of
      Just varState ->
        if not (Set.null (values varState))
          then KnownValues (values varState)  -- explicit values exist
          else BoundedValues (low_b varState) (upp_b varState)  -- otherwise, we use inferred bounds
      Nothing -> BoundedValues Nothing Nothing
    Nothing -> BoundedValues Nothing Nothing

inferValues (Add e1 e2) nameToID varStates =
  case (inferValues e1 nameToID varStates, inferValues e2 nameToID varStates) of
    -- Case 1: both have explicit values
    (KnownValues v1, KnownValues v2) -> KnownValues (Set.fromList [x + y | x <- Set.toList v1, y <- Set.toList v2])

    -- Case 2: both have only bounds
    (BoundedValues (Just lb1) (Just ub1), BoundedValues (Just lb2) (Just ub2)) ->
        BoundedValues (Just (lb1 + lb2)) (Just (ub1 + ub2))

    -- Case 3: one side has explicit values, the other has bounds
    (KnownValues v1, BoundedValues (Just lb2) (Just ub2)) ->
        KnownValues (Set.fromList [x + y | x <- Set.toList v1, y <- [lb2..ub2]])
    (BoundedValues (Just lb1) (Just ub1), KnownValues v2) ->
        KnownValues (Set.fromList [x + y | x <- [lb1..ub1], y <- Set.toList v2])

    -- Case 4: if either bound is missing, return unknown bounds
    _ -> BoundedValues Nothing Nothing

inferValues (Sub e1 e2) nameToID varStates =
  case (inferValues e1 nameToID varStates, inferValues e2 nameToID varStates) of
    -- Case 1: both have explicit values
    (KnownValues v1, KnownValues v2) -> KnownValues (Set.fromList [x - y | x <- Set.toList v1, y <- Set.toList v2])

    -- Case 2: both have only bounds
    (BoundedValues (Just lb1) (Just ub1), BoundedValues (Just lb2) (Just ub2)) ->
        BoundedValues (Just (lb1 - ub2)) (Just (ub1 - lb2))  -- Min subtraction gives new lower bound, max gives upper bound

    -- Case 3: one side has explicit values, the other has bounds
    (KnownValues v1, BoundedValues (Just lb2) (Just ub2)) ->
        KnownValues (Set.fromList [x - y | x <- Set.toList v1, y <- [lb2..ub2]])
    (BoundedValues (Just lb1) (Just ub1), KnownValues v2) ->
        KnownValues (Set.fromList [x - y | x <- [lb1..ub1], y <- Set.toList v2])

    -- Case 4: if either bound is missing, return unknown bounds
    _ -> BoundedValues Nothing Nothing

inferValues (Mul e1 e2) nameToID varStates =
  case (inferValues e1 nameToID varStates, inferValues e2 nameToID varStates) of
    -- Case 1: both have explicit values
    (KnownValues v1, KnownValues v2) -> KnownValues (Set.fromList [x * y | x <- Set.toList v1, y <- Set.toList v2])

    -- Case 2: both have only bounds
    (BoundedValues (Just lb1) (Just ub1), BoundedValues (Just lb2) (Just ub2)) ->
        BoundedValues (Just (lb1 * lb2)) (Just (ub1 * ub2))

    -- Case 3: one side has explicit values, the other has bounds
    (KnownValues v1, BoundedValues (Just lb2) (Just ub2)) ->
        KnownValues (Set.fromList [x * y | x <- Set.toList v1, y <- [lb2..ub2]])
    (BoundedValues (Just lb1) (Just ub1), KnownValues v2) ->
        KnownValues (Set.fromList [x * y | x <- [lb1..ub1], y <- Set.toList v2])

    -- Case 4: if either bound is missing, return unknown bounds
    _ -> BoundedValues Nothing Nothing

inferValues _ _ _ = BoundedValues Nothing Nothing -- TODO: Handle other cases properly


-- Helper function: Computes modular inverse
modularInverse :: Integer -> Integer
modularInverse c = if c /= 0 then 1 `div` c else error "Division by zero"
-- TODO: Implement modular inverse properly for field arithmetic.

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
   xID <- case Map.lookup xName nameToID of
             Just vid -> pure vid
             Nothing  -> Left ("Unknown var: " ++ xName)
   yID <- case Map.lookup yName nameToID of
             Just vid -> pure vid
             Nothing  -> Left ("Unknown var: " ++ yName)

   let xState = maybe
                  (Left ("No VariableState for " ++ xName))
                  Right
                  (Map.lookup xID oldVarStates)
   let yState = maybe
                  (Left ("No VariableState for " ++ yName))
                  Right
                  (Map.lookup yID oldVarStates)

   xSt <- xState
   ySt <- yState

   -- checking if x is already known to be in [0,1]
   if isIn01 xSt then do
     -- if so, setting y to [0,1]
     newY <- updateValues ySt (BoundedValues (Just 0) (Just 1))
     let changedY = newY /= ySt
         newMapY  = if changedY
                    then Map.insert yID newY oldVarStates
                    else oldVarStates
     pure (changedY, newMapY)

   -- otherwise, checking if y is already known to be in [0,1]
   else if isIn01 ySt then do
     -- if so, setting x to [0,1]
     newX <- updateValues xSt (BoundedValues (Just 0) (Just 1))
     let changedX = newX /= xSt
         newMapX  = if changedX
                    then Map.insert xID newX oldVarStates
                    else oldVarStates
     pure (changedX, newMapX)

   else
     -- no update if neither is already in [0,1].
     pure (False, oldVarStates)

-- Helper function: checks whether a VariableState is restricted to exactly [0,1].
isIn01 :: VariableState -> Bool
isIn01 (VariableState vals lowB uppB _)
    -- if we have an explicit set of possible values & it is subset of {0,1}
    | not (Set.null vals) =
        let allZeroOne = Set.isSubsetOf vals (Set.fromList [0,1])
        in allZeroOne

    -- otherwise, if we only have bounding info, we check if lb>=0 and ub<=1
    | otherwise =
        case (lowB, uppB) of
          (Just lb, Just ub) -> lb >= 0 && ub <= 1
          _ -> False

-- | Applies an "interesting" constraint to update variable states.
-- TODO: OrC, ... !!
analyzeConstraint :: Constraint -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)


-- ALREADY HANDLED BY THE ROOT RULE:
--analyzeConstraint (EqC cid (Var xName) (Int 0)) nameToID varStates =
--  markVarDefinitelyZero xName nameToID varStates

--analyzeConstraint (EqC cid (Int 0) (Var xName)) nameToID varStates =
--  markVarDefinitelyZero xName nameToID varStates

-- | Rule 4a from Ecne
analyzeConstraint (EqC _ (Var xName) (Var yName)) nameToID varStates =
  case (Map.lookup xName nameToID, Map.lookup yName nameToID) of
    (Just xID, Just yID) ->
      case (Map.lookup xID varStates, Map.lookup yID varStates) of
        (Just xState, Just yState) ->
          let xValues = values xState
              yValues = values yState
              bothEmpty = Set.null xValues && Set.null yValues
              oneEmpty = Set.null xValues || Set.null yValues
          in if bothEmpty
             then Right (False, varStates)  -- nothing to propagate
             else if oneEmpty
                  then let newValues = Set.union xValues yValues
                           newXState = xState { values = newValues }
                           newYState = yState { values = newValues }
                           newVarStates = Map.insert xID newXState $
                                          Map.insert yID newYState varStates
                       in Right (True, newVarStates)  -- transferring values
                  else -- both have values, checking for consistency
                    case updateValues xState (KnownValues yValues) >>= \updatedX ->
                         updateValues yState (KnownValues xValues) >>= \updatedY ->
                         Right (updatedX, updatedY) of
                      Right (updatedX, updatedY) ->
                        let changed = updatedX /= xState || updatedY /= yState
                            newVarStates = Map.insert xID updatedX $
                                           Map.insert yID updatedY varStates
                        in Right (changed, newVarStates)
                      Left errMsg -> Left errMsg
        _ -> Right (False, varStates)  -- variables not initialized
    _ -> Left "Variable name not found in nameToID map"

-- ASSIGN Rule from PICUS paper
analyzeConstraint (EqC _ (Mul (Int c) (Var xName)) e) nameToID varStates
  | c /= 0 =
      let omega = inferValues e nameToID varStates
          cInv = modularInverse c
          newVals = case omega of
                      KnownValues vSet -> KnownValues (Set.map (* cInv) vSet)
                      BoundedValues (Just lb) (Just ub) ->
                        BoundedValues (Just (lb * cInv)) (Just (ub * cInv))
                      _ -> BoundedValues Nothing Nothing
      in case Map.lookup xName nameToID of
           Nothing -> Left "Variable name not found in nameToID"
           Just xID ->
             case Map.lookup xID varStates of
               Nothing -> Left "Variable state not found in varStates"
               Just xState ->
                 case updateValues xState newVals of
                   Right updatedState ->
                     let changed = xState /= updatedState -- if changes, need to re-queue constraints
                         updatedMap = if changed then Map.insert xID updatedState varStates else varStates
                     in Right (changed, updatedMap)
                   Left errMsg -> Left errMsg
  | otherwise = Right (False, varStates)

-- | ROOT Rule from PICUS paper
analyzeConstraint (EqC _ rootExpr (Int 0)) nameToID varStates =
    case extractRootFactors rootExpr of
        Just (xName, rootValues) ->
            case Map.lookup xName nameToID of
                Just xID ->
                    case Map.lookup xID varStates of
                        Just xState ->
                            let newVals = Set.fromList rootValues
                            in case updateValues xState (KnownValues newVals) of
                                Right updatedState ->
                                    let changed = xState /= updatedState
                                        updatedMap = if changed then Map.insert xID updatedState varStates else varStates
                                    in Right (changed, updatedMap)
                                Left errMsg -> Left errMsg
                        Nothing -> Left "Variable state not found in varStates"
                Nothing -> Left "Variable name not found in nameToID"
        Nothing -> Right (False, varStates)  -- not a ROOT constraint

-- | Rule 3 from Ecne
--    Sum-of-powers rule:  EqC cid (some expression) (Var zName)
--    If that expression is c^k * b_k + ... + c^m * b_m with each b in [0,1],
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
               c       = 2  -- TODO: generalize
               upBound = c^(maxExp + 1) - 1
               zID     = case Map.lookup zName nameToID of
                           Just i  -> i
                           Nothing -> error ("No var ID for " ++ zName)
               zState  = case Map.lookup zID varStates of
                           Just st -> st
                           Nothing -> error ("No VariableState for " ++ show zID)

           -- 3) we update the variable 'z' with [0, upBound]
           updatedZState <- -- TODO: check for inconsistencies
             updateValues zState (BoundedValues (Just 0) (Just upBound))

           let changed1   = updatedZState /= zState
               varStates1 = Map.insert zID updatedZState varStates

           -- 4) if 'z' is exactly known => we can infer the bits
           let zVals = values updatedZState
           if Set.size zVals == 1
              then do
                let knownZ = head (Set.toList zVals)
                varStates2 <- decodeSumOfPowers 2 knownZ terms nameToID varStates1 -- TODO: generalize
                pure (True, varStates2)
              else pure (changed1, varStates1)

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
    --            left-biased Either: if any sub-constraint fails, we propagate the error.
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

-- TODO: handle the symmetrical case:  EqC cid (Var zName) rhs
-- if checkSumOfPowers 2 rhs = ...

-- | NonZero rule: a * b + 1 = 0 then a is nonzero and b is nonzero.
--   same for a * b = 1
-- TODO: rekening houden met prime field!
analyzeConstraint (EqC cid (Add (Var zName) (Mul (Var xName) (Var yName))) (Int c)) nameToID varStates =
    case Map.lookup zName nameToID of
     Nothing -> Right (False, varStates)
     Just zID ->
       case Map.lookup zID varStates of
         Nothing -> Right (False, varStates)
         Just zState ->
           if isCertainlyZero zState && c /= 0
            then markNonZeroPair xName yName nameToID varStates
            else
              Right (False, varStates)
--analyzeConstraint (EqC cid (Int c) (Mul (Var xName) (Var yName))) nameToID varStates =
--     if c /= 0 then markNonZeroPair xName yName nameToID varStates
--      else Right (False, varStates)
analyzeConstraint (EqC cid (Add (Mul (Var xName) (Var yName)) (Int c1)) (Int c2)) nameToID varStates =
     if c1 /= 0 && c2 == 0 then markNonZeroPair xName yName nameToID varStates
      else Right (False, varStates)
analyzeConstraint (EqC cid (Mul (Var xName) (Var yName)) (Int c)) nameToID varStates =
    if c /= 0 then markNonZeroPair xName yName nameToID varStates
      else Right (False, varStates)

analyzeConstraint _ _ varStates = Right (False, varStates)  -- TODO: handle other constraints


-- Helper function
isCertainlyZero :: VariableState -> Bool
isCertainlyZero st =
    not (nonZero st)
      && Set.size (values st) == 1
      && Set.member 0 (values st)

markVarDefinitelyZero :: String -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)
markVarDefinitelyZero xName nameToID varStates = do
  xID <- case Map.lookup xName nameToID of
            Just i  -> Right i
            Nothing -> Left $ "Unknown variable " ++ xName
  oldSt <- case Map.lookup xID varStates of
             Just s  -> Right s
             Nothing -> Left $ "No VariableState for varID=" ++ show xID

  let newSt = oldSt { values  = Set.singleton 0
                    , nonZero = False }
      changed = newSt /= oldSt
      newMap  = if changed
                then Map.insert xID newSt varStates
                else varStates

  pure (changed, newMap)

markNonZeroPair :: String -> String -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)
markNonZeroPair xName yName nameToID varStates = do
    xID <- lookupID xName
    yID <- lookupID yName
    oldXSt <- lookupVarState xID
    oldYSt <- lookupVarState yID

    let newXSt = if not (nonZero oldXSt) then setNonZero oldXSt else oldXSt
        newYSt = if not (nonZero oldYSt) then setNonZero oldYSt else oldYSt

        changed = newXSt /= oldXSt || newYSt /= oldYSt
        newMap  = (Map.insert xID newXSt . Map.insert yID newYSt) varStates

    pure (changed, newMap)

  where
    lookupID nm =
      case Map.lookup nm nameToID of
        Just i  -> Right i
        Nothing -> Left ("Unknown variable " ++ nm)

    lookupVarState vid =
      case Map.lookup vid varStates of
        Just s  -> Right s
        Nothing -> Left ("No VariableState for varID=" ++ show vid)

-- | We try to convert `expr` into a sum of terms: c^exponent * Var(b).
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

    go (Mul (Int k) (Var bName)) = do
      expK <- isExactPowerOf c k  -- e.g. if c=2, k=4 => exponent=2
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
  case Map.lookup varName nameToID of
    Nothing   -> False
    Just vid  ->
      case Map.lookup vid varStates of
        Nothing -> False
        Just st ->
          (Set.null (values st) && Just 0 == low_b st && Just 1 == upp_b st)
          || (values st == Set.fromList [0,1])

-- | If z is single-valued, we decode each (bName, exponent)
--   so that bName ∈ {0,1} matches the corresponding 'bit' in base c.
decodeSumOfPowers
  :: Integer                    -- c, the base
  -> Integer                    -- known value of z
  -> [(String, Integer)]        -- (bName, exponent) pairs
  -> Map String Int             -- nameToID
  -> Map Int VariableState      -- varStates
  -> Either String (Map Int VariableState)
decodeSumOfPowers c z terms nameToID varStates0 =
  foldl step (Right varStates0) terms
  where
    step (Left err) _ = Left err
    step (Right vs) (bName, e) =
      case Map.lookup bName nameToID of
        Nothing   -> Left ("No varID for " ++ bName)
        Just bID  ->
          case Map.lookup bID vs of
            Nothing -> Left ("No VariableState for " ++ show bID)
            Just st ->
              let coeff   = c^e
                  digit   = (z `div` coeff) `mod` c  -- extracting the base-c digit
                  bValSet = if digit == 1 then Set.singleton 1
                                          else Set.singleton 0
              in case updateValues st (KnownValues bValSet) of
                   Left msg         -> Left msg
                   Right updatedSt  ->
                     let vs' = Map.insert bID updatedSt vs
                     in Right vs'

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

analyzeConstraints :: Map Int Constraint -> Map String Int -> Map Int [Int] -> Map Int VariableState -> Map String VariableState
analyzeConstraints constraints nameToID varToConstraints = loop (initializeQueue (Map.elems constraints))
  where
    loop :: Seq Int -> Map Int VariableState -> Map String VariableState
    loop queue vStates =
      case viewl queue of
        Seq.EmptyL -> transformIDToNames nameToID vStates
        cId :< restQueue ->
          case Map.lookup cId constraints of
            Just constraint ->
              case analyzeConstraint constraint nameToID vStates of
                Right (changed, newVarStates) ->
                  let newQueue = if changed
                            -- re-queue all constraints that contain the affected variables
                             then let affectedVars = collectVarsFromConstraint nameToID constraint
                                  in foldl (|>) restQueue (concatMap (\v -> Map.findWithDefault [] v varToConstraints) affectedVars)
                             else restQueue
                  in loop newQueue newVarStates
                Left errMsg -> error errMsg
            Nothing -> loop restQueue vStates

--------------------------
-- 6) Main Analysis
--------------------------

analyzeProgram :: Program -> Map String VariableState
analyzeProgram (Program inputs compVars constrVars _ constraints) =
  let nameToID = buildVarNameToIDMap (inputs ++ compVars ++ constrVars)
      varStates = initializeVarStates (inputs ++ compVars ++ constrVars)
      varToConstraints = buildVarToConstraints nameToID constraints
      constraintMap = Map.fromList [(getConstraintID c, c) | c <- constraints]
  in analyzeConstraints constraintMap nameToID varToConstraints varStates

--------------------------------------------------------------------------------
-- 7) Bug Detection
--------------------------------------------------------------------------------

{- 
  | This function:
   1) runs 'analyzeProgram' on the Program to get final states 
   2) for each variable in Program, examines its VariableState 
   3) checks whether it is consistent with the variable's declared Sort 
-}

detectBugs :: Program -> Either [String] ()
detectBugs program =
  let varStates = analyzeProgram program

      -- we gather errors for each variable
      errors = concatMap (checkVariable varStates) allVars
      allVars = inputs program
                 ++ computationVars program
                 ++ constraintVars program

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
      let errs = checkSort (sort binding) vState (name binding)
      in errs

-- | Checks that the final VariableState is consistent with the Sort.
--   Returns list of errors if any.
checkSort :: Sort -> VariableState -> String -> [String]
checkSort Bool vs varName         = checkBoolean vs varName
checkSort (BitVector n) vs varName= checkMaxVal ((2 ^ n) - 1) vs varName
checkSort (FieldMod p) vs varName = checkMaxVal (p - 1) vs varName
checkSort NonZero vs varName      = checkNonZero vs varName

-- Checking Booleans

{- 
   For a boolean variable, the final possibilities can be:

   1) explicit set {0,1}, or subset, or 
   2) an explicit set [some range], but it must not exceed [0..1].

   If 'values' is non-empty, we check that the set is ⊆ {0,1}.
   If 'values' is empty, but we have low_b / upp_b, we check those. 
-}
checkBoolean :: VariableState -> String -> [String]
checkBoolean (VariableState vals lowB upB _) varName =
  let
    possibleVals =
      if not (Set.null vals)
        -- we have explicit enumerated possibilities
        then vals
        else case (lowB, upB) of
                -- we have bounds
               (Just lb, Just ub) -> Set.fromList [lb..ub]
               -- unknown
               _ -> Set.empty
    -- if we ended up with an empty set, that's an error:
    noValuesError =
      [ "Boolean variable `" ++ varName
        ++ "` has no possible values (unconstrained)." 
      | Set.null possibleVals ]

    -- otherwise, we check whether any values are outside of {0,1}:
    invalidVals = Set.filter (\v -> v /= 0 && v /= 1) possibleVals
    invalidValsError =
      [ "Boolean variable `" ++ varName
        ++ "` has values outside {0,1}: " ++ show (Set.toList invalidVals)
      | not (Set.null invalidVals) ]

  in noValuesError ++ invalidValsError

-- Checking BitVectors and FieldMods

{- 
   For a variable declared (BitVector n), 
   we want to check that all final possible values are in [0 .. 2^n - 1], 
   or if it has bounds, then the upper bound must not exceed 2^n - 1.
-}
checkMaxVal :: Integer -> VariableState -> String -> [String]
checkMaxVal maxVal (VariableState vals lowB upB _) varName = 
  let
    -- if we have enumerated values, we use them; otherwise, we gather from bounds
    possibleVals = -- TODO: fix code duplication, maak functie allVals
      if not (Set.null vals)
        then vals
        else case (lowB, upB) of
               (Just lb, Just ub) -> Set.fromList [lb..ub]
               _                  -> Set.empty

    -- if empty domain -> error
    noValuesError =
      [ "Variable `" ++ varName
        ++ "` has no possible values (unconstrained)."
      | Set.null possibleVals ]

    -- If not empty, check out-of-range
    invalidVals = Set.filter (\v -> v < 0 || v > maxVal) possibleVals
    rangeError =
      [ "Variable `" ++ varName
        ++ "` has out-of-range values: " ++ show (Set.toList invalidVals)
      | not (Set.null invalidVals) ]

    in noValuesError ++ rangeError  

-- Checking NonZero

{- 
   If a variable is declared NonZero, it must not contain 0 in its final set. 
   Or the vState.nonZero should be True. 
   Or, if no explicit values are available, 
   we check if bounds do not include 0. TODO: to think, we kijken best enkel of het nonZero is?
-}
checkNonZero :: VariableState -> String -> [String]
checkNonZero (VariableState vals lowB upB nonZ) varName =
  let
    -- if we have enumerated vals:
    possibleVals =
      if not (Set.null vals)
        then vals
        else case (lowB, upB) of
               (Just lb, Just ub) -> Set.fromList [lb..ub]
               _                  -> Set.empty

    msgNonZeroFlag =
      (["Variable `" ++ varName ++ "` declared NonZero but varState.nonZero == False" | not nonZ])

    msgZeroInVals =
      (["Variable `" ++ varName ++ "` declared NonZero but 0 is in possible set: "
                    ++ show (Set.toList possibleVals) | 0 `Set.member` possibleVals])
  in msgNonZeroFlag ++ msgZeroInVals