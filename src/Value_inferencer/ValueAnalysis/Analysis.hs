{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module ValueAnalysis.Analysis (analyzeProgram, VariableState(..), detectBugs, updateValues, initVarState, ValueDomain(..), analyzeFromFile) where

import Syntax.AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, (|>), viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)
import Syntax.Compiler (parseAndCompile)
import Data.List (intercalate)
import ValueAnalysis.UserRules
import ValueAnalysis.VariableState
import ValueAnalysis.ValueDomain

--------------------------
-- 1) Variable State Representation
--------------------------

-- Moved to separate file

--------------------------
-- 2) Initializing Variable States
--------------------------

-- | Builds a map from variable IDs to their initial state.
initializeVarStates :: [Binding] -> Map Int VariableState
initializeVarStates vars = Map.fromList [(vid v, initVarState) | v <- vars]

-- | Builds a map from variable names to their IDs for lookup.
-- TODO: just replace all vars in constraints by their ID during compilation!
buildVarNameToIDMap :: [Binding] -> Map String Int
buildVarNameToIDMap vars = Map.fromList [(name v, vid v) | v <- vars]

-- Helper: Lookup variable ID by name
lookupVarID :: String -> Map String Int -> Either String Int
lookupVarID name nameToID =
  case Map.lookup name nameToID of
    Just vID -> Right vID
    Nothing  -> Left $ "Variable name not found in nameToID map: " ++ name

-- Helper: Lookup variable state by ID
lookupVarState :: Int -> Map Int VariableState -> Either String VariableState
lookupVarState vID varStates =
  case Map.lookup vID varStates of
    Just state -> Right state
    Nothing    -> Left $ "Variable state not found in varStates for ID: " ++ show vID

-- Helper: Lookup variable state by name
lookupVarStateByName :: String -> Map String Int -> Map Int VariableState -> Either String VariableState
lookupVarStateByName name nameToID varStates = do
  vID <- lookupVarID name nameToID
  lookupVarState vID varStates

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
--   It does this by intersecting the existing domain with new domain information.
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

getVarID = Map.lookup

expandBounds :: Maybe Integer -> Maybe Integer -> [Integer]
expandBounds (Just lb) (Just ub) = [lb .. ub]  -- expands into a list
expandBounds _ _ = []  -- no meaningful bounds

-- | Recursively infers possible values of an expression
-- TODO: fix code duplication
inferValues :: Expression -> Map String Int -> Map Int VariableState -> ValueDomain
inferValues (Int c) _ _ = KnownValues (Set.singleton c)

inferValues (Var xName) nameToID varStates =
  case Map.lookup xName nameToID of
    -- If we found the variable state, we return the domain from the state.
    -- Variable known but no state? We return default domain.
    Just varID -> maybe defaultValueDomain domain (Map.lookup varID varStates)
    -- Variable name not found? We return default domain.
    Nothing -> defaultValueDomain 

inferValues (Add e1 e2) nameToID varStates =
    let d1 = inferValues e1 nameToID varStates
        d2 = inferValues e2 nameToID varStates
    in case (d1, d2) of
       (KnownValues s1, KnownValues s2) ->
         KnownValues (Set.fromList [x + y | x <- Set.toList s1, y <- Set.toList s2])
       (BoundedValues lb1 ub1 ex1, BoundedValues lb2 ub2 ex2) ->
         -- we use <$> to add only if the bounds are Just
         let newLb = (+) <$> lb1 <*> lb2 
             newUb = (+) <$> ub1 <*> ub2 
             -- TODO: we currently sum the exclusions, which seems fine thinking about
             -- our main goal of detecting division-by-zero. However, we have to
             -- verify whether this approach makes sense for general cases.
             newEx = combineExclusions ex1 ex2 
         in BoundedValues newLb newUb (if Set.null newEx then Nothing else Just newEx)

       -- mixed cases (KnownValues + BoundedValues)
       -- we combine them into BoundedValues
       (KnownValues s, BoundedValues lb ub excl) ->
         if Set.null s 
          then defaultValueDomain 
          else
            let minS = Set.findMin s
                maxS = Set.findMax s
                newLb = (+) <$> Just minS <*> lb
                newUb = (+) <$> Just maxS <*> ub
            in BoundedValues newLb newUb excl;
       (BoundedValues lb ub excl, KnownValues s) ->
         if Set.null s 
          then defaultValueDomain 
          else
            let minS = Set.findMin s
                maxS = Set.findMax s
                newLb = (+) <$> lb <*> Just minS
                newUb = (+) <$> ub <*> Just maxS
            in BoundedValues newLb newUb excl

inferValues (Sub e1 e2) nameToID varStates =
  let d1 = inferValues e1 nameToID varStates
      d2 = inferValues e2 nameToID varStates
  in case (d1, d2) of
       (KnownValues s1, KnownValues s2) ->
         KnownValues (Set.fromList [x - y | x <- Set.toList s1, y <- Set.toList s2])
       (BoundedValues lb1 ub1 ex1, BoundedValues lb2 ub2 ex2) ->
         -- new lower Bound = lb1 - ub2
         -- new upper Bound = ub1 - lb2
         let newLb = (-) <$> lb1 <*> ub2
             newUb = (-) <$> ub1 <*> lb2
             newEx = combineExclusions ex1 ex2
         in BoundedValues newLb newUb (if Set.null newEx then Nothing else Just newEx)
       -- mixed cases
       (KnownValues s, BoundedValues lb ub excl) ->
         if Set.null s then defaultValueDomain else
         let minS = Set.findMin s
             maxS = Set.findMax s
             newLb = (-) <$> Just minS <*> ub
             newUb = (-) <$> Just maxS <*> lb
         in BoundedValues newLb newUb excl
       (BoundedValues lb ub excl, KnownValues s) ->
         if Set.null s then defaultValueDomain else
         let minS = Set.findMin s
             maxS = Set.findMax s
             newLb = (-) <$> lb <*> Just maxS 
             newUb = (-) <$> ub <*> Just minS
         in BoundedValues newLb newUb excl

inferValues (Mul e1 e2) nameToID varStates =
  let d1 = inferValues e1 nameToID varStates
      d2 = inferValues e2 nameToID varStates
  in case (d1, d2) of
       (KnownValues s1, KnownValues s2) ->
         KnownValues (Set.fromList [x * y | x <- Set.toList s1, y <- Set.toList s2])
       (BoundedValues (Just lb1) (Just ub1) ex1, BoundedValues (Just lb2) (Just ub2) ex2) ->
         let newLb = Just (lb1 * lb2)
             newUb = Just (ub1 * ub2)
             newEx = combineExclusions ex1 ex2
         in BoundedValues newLb newUb (if Set.null newEx then Nothing else Just newEx)
       -- if any bound is missing in Bounded*Bounded, result is unknown bounds
       -- TODO: kan misschien slimmer? 
       (BoundedValues _ _ ex1, BoundedValues _ _ ex2) ->
         let newEx = combineExclusions ex1 ex2
         in BoundedValues Nothing Nothing (if Set.null newEx then Nothing else Just newEx)
       -- mixed cases
       (KnownValues s, BoundedValues (Just lb) (Just ub) excl) ->
         if Set.null s then defaultValueDomain else
         let products = [v * b | v <- Set.toList s, b <- [lb, ub]]
             newLb = Just (minimum products)
             newUb = Just (maximum products)
         in BoundedValues newLb newUb excl
       (BoundedValues (Just lb) (Just ub) excl, KnownValues s) ->
         if Set.null s then defaultValueDomain else
         let products = [b * v | b <- [lb, ub], v <- Set.toList s]
             newLb = Just (minimum products)
             newUb = Just (maximum products)
         in BoundedValues newLb newUb excl
       -- if bounds are missing in mixed case, result is unknown bounds
       -- TODO: kan misschien slimmer? 
       _ -> BoundedValues Nothing Nothing Nothing

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
isIn01 st = case domain st of
    KnownValues s -> Set.isSubsetOf s (Set.fromList [0, 1])
    BoundedValues (Just lb) (Just ub) maybeEx ->
        lb >= 0 && ub <= 1 &&
        -- ensuring 0 and 1 are not both excluded if the range is exactly [0, 1]
        not (lb == 0 && ub == 1 && maybe False (Set.isSupersetOf (Set.fromList [0,1])) maybeEx) &&
        -- ensuring the single value isn't excluded if lb == ub
        not (lb == ub && maybe False (Set.member lb) maybeEx)
    BoundedValues (Just lb) Nothing _ -> False -- cannot be [0,1] if upper bound is open
    BoundedValues Nothing (Just ub) _ -> False -- cannot be [0,1] if lower bound is open
    BoundedValues Nothing Nothing _ -> False -- cannot be [0,1] if bounds are unknown

-- | Applies an "interesting" constraint to update variable states.
-- TODO: OrC, ... !!
analyzeConstraint :: Constraint -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)

-- EQUALITY Rule: x = y
analyzeConstraint (EqC _ (Var xName) e) nameToID varStates =
  case e of
    -- | Rule 4a from Ecne: x = y
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
      let omega = inferValues e nameToID varStates -- inferring domain of expression e
      -- updating x's state by intersecting its current domain with the inferred domain of e
      case updateValues xState omega of
        Right updatedState -> do
          let changed = xState /= updatedState -- checking if the state actually changed
              updatedMap = if changed then Map.insert xID updatedState varStates else varStates
          return $ Right (changed, updatedMap)
        Left errMsg -> return $ Left errMsg -- intersection failed, contradiction

-- ALREADY HANDLED BY THE ROOT RULE:
--analyzeConstraint (EqC cid (Var xName) (Int 0)) nameToID varStates =
--  markVarDefinitelyZero xName nameToID varStates

--analyzeConstraint (EqC cid (Int 0) (Var xName)) nameToID varStates =
--  markVarDefinitelyZero xName nameToID varStates  

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
           updatedZState <-
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
    newXStEither <- updateValues oldXSt (excludeZero (domain oldXSt))
    -- trying to update y by excluding zero
    newYStEither <- updateValues oldYSt (excludeZero (domain oldYSt))

    -- handling potential errors from updateValues
    newXSt <- newXStEither
    newYSt <- newYStEither

    let changed = newXSt /= oldXSt || newYSt /= oldYSt
        -- applying updates only if they succeeded
        newMap = Map.insert xID newXSt (Map.insert yID newYSt varStates)

    pure (changed, newMap)

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

-- | Nicely prints the analysis results.
prettyPrintStore :: Map String VariableState -> IO ()
prettyPrintStore store = do
    mapM_ printVariable (Map.toList store)
  where
    printVariable (varName, VariableState values low_b upp_b _) = do
        putStrLn $ "Variable: " ++ varName
        putStr "- Inferred Values: "
        case determineState values low_b upp_b of
            Left explicitValues -> putStrLn $ "{" ++ intercalate ", " (map show (Set.toList explicitValues)) ++ "}"
            Right (Just lb, Just ub) -> putStrLn $ "[" ++ show lb ++ ", " ++ show ub ++ "]"
            Right _ -> putStrLn "Unknown"
        putStrLn ""

    -- determines whether to display explicit values, bounds, or unknown.
    determineState :: Set Integer -> Maybe Integer -> Maybe Integer -> Either (Set Integer) (Maybe Integer, Maybe Integer)
    determineState vals lb ub
        | not (Set.null vals) = Left vals  -- explicit values 
        | otherwise           = Right (lb, ub)  -- otherwise, we display bounds or unknown


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
      let oldState = Map.findWithDefault initVarState realID varStates
          newState = updateValues oldState (KnownValues enumer)
      in case newState of
                   Left msg         -> (False, varStates)
                   Right newState  -> (True, Map.insert realID newState varStates)

applyUserAction (ConstrainRange placeholderName lo up) plHoNameToID varStates =
  case Map.lookup placeholderName plHoNameToID of
    Nothing -> (False, varStates)
    Just realID ->
      let oldState = Map.findWithDefault initVarState realID varStates
          newState = updateValues oldState (BoundedValues (Just lo) (Just up))
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
      -- we gather errors for each variable
      sortErrors = concatMap (checkVariable varStates) vars
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
      let errs = checkSort (sort binding) vState (name binding)
      in errs

-- | Checks that the final VariableState is consistent with the Sort.
--   Returns list of errors if any.
checkSort :: Sort -> VariableState -> String -> [String]
checkSort Bool vs varName         = checkBoolean vs varName
checkSort (BitVector n) vs varName= checkMaxVal ((2 ^ n) - 1) vs varName
checkSort (FieldMod p) vs varName = checkMaxVal (p - 1) vs varName

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
        ++ "` has no possible values (unconstrained)"
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
        ++ "` has no possible values (unconstrained)"
      | Set.null possibleVals ]

    -- If not empty, check out-of-range
    invalidVals = Set.filter (\v -> v < 0 || v > maxVal) possibleVals
    rangeError =
      [ "Variable `" ++ varName
        ++ "` has out-of-range values: " ++ show (Set.toList invalidVals)
      | not (Set.null invalidVals) ]

    in noValuesError ++ rangeError

-- | Returns 'True' if the variable is guaranteed to be non-zero.
isVarNonZero :: String -> Map String VariableState -> Bool
isVarNonZero xName st =
  maybe False nonZero (Map.lookup xName st)

-- | Checks if any PfRecip expression could be zero "at runtime", more precisely :
--   1) if the expression is constrained to be nonZero (via nonZero flag)
--   2) or, 0 is not in the expression's value domain
checkPfRecips :: [Expression] -> Map String VariableState -> Map String Int -> [String]
checkPfRecips denominators store nameToID =
  concatMap checkSingle denominators
  where
    checkSingle expr
      | expressionIsDefinitelyNonZero expr store = []
      | otherwise =
          let domain = inferValues expr nameToID (invertStates store nameToID)
          in case domain of
               KnownValues vSet ->
                 [ "Denominator expression `" ++ show expr ++ "` may be 0!"
                 | 0 `Set.member` vSet
                 ]
               BoundedValues (Just lb) (Just ub) ->
                 [ "Denominator expression `" ++ show expr ++ "` may be 0!"
                 | lb <= 0 && ub >= 0
                 ]
               _ -> []

-- | Simple non-zero inferencer.
--   Returns True if the expression is constrained to be nonZero.
expressionIsDefinitelyNonZero :: Expression -> Map String VariableState -> Bool
-- Case 1: literal integer
expressionIsDefinitelyNonZero (Int c) _ = c /= 0

-- Case 2: single variable
expressionIsDefinitelyNonZero (Var xName) st =
  isVarNonZero xName st

-- Case 3: number * var
-- if the multiplier is non-zero and the variable is definitely non-zero, the product is non-zero
expressionIsDefinitelyNonZero (Mul (Int c) (Var xName)) st
  | c /= 0 = isVarNonZero xName st
  | otherwise = False
expressionIsDefinitelyNonZero (Mul (Var xName) (Int c)) st =
  expressionIsDefinitelyNonZero (Mul (Int c) (Var xName)) st

-- Case 4: var + number
-- if the variable is definitely non-zero, we assume the sum is non-zero
-- TODO: wat met overflows? die worden al gedetecteerd wss?
expressionIsDefinitelyNonZero (Add (Var xName) (Int c)) st = isVarNonZero xName st
expressionIsDefinitelyNonZero (Add (Int c) (Var xName)) st =
  expressionIsDefinitelyNonZero (Add (Var xName) (Int c)) st

-- Case 5: fallback, cannot guarantee non-zero
expressionIsDefinitelyNonZero _ _ = False

-- | Converts String->VariableState to Int->VariableState so we can call inferValues.
invertStates :: Map String VariableState -> Map String Int -> Map Int VariableState
invertStates st nmToID =
  Map.fromList
    [ (nmToID Map.! varName, vState)
    | (varName, vState) <- Map.toList st
    , varName `Map.member` nmToID
    ]