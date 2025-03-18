{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module ValueAnalysis.Analysis (analyzeProgram) where

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
    upp_b :: Maybe Integer
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
    upp_b = Nothing
  }  

-- | Builds a map from variable IDs to their initial state.
initializeVarStates :: [Binding] -> Map Int VariableState
initializeVarStates vars = Map.fromList [(vid v, initVarState) | v <- vars]

-- | Builds a map from variable names to their IDs for lookup.
-- TODO: just replace all vars in constraints by their ID during compilation!
buildVarNameToIDMap :: [Binding] -> Map String Int
buildVarNameToIDMap vars = Map.fromList [(name v, vid v) | v <- vars]

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
  -- if values are already known, we check for consistency
  | not (Set.null (values vState)) =
      if Set.isSubsetOf newVals (values vState)
      then Right vState { values = newVals }  -- Safe update
      else Left $ "Inconsistent value inference! New values " ++ show newVals
                ++ " are not a subset of existing " ++ show (values vState)
  -- if no explicit values, we infer from bounds or assign directly
  | otherwise =
      let lowerBound = fromMaybe (minimum newVals) (low_b vState)
          upperBound = fromMaybe (maximum newVals) (upp_b vState)
          inferredVals = Set.fromList [lowerBound .. upperBound]
      in if Set.isSubsetOf newVals inferredVals
         then Right vState { values = newVals }  -- Safe update
         else Left $ "Value inference contradicts bounds! New values: "
                  ++ show newVals ++ " do not fit in inferred range "
                  ++ show (Set.toList inferredVals)

updateValues vState (BoundedValues (Just lb) (Just ub))
  -- if values are already known, we check consistency
  | not (Set.null (values vState)) =
      let inferredVals = Set.fromList [lb .. ub]
      in if Set.isSubsetOf (values vState) inferredVals
         then Right vState  -- Safe, values already fit
         else Left $ "Existing values " ++ show (values vState)
                  ++ " do not fit in inferred range [" ++ show lb ++ ", " ++ show ub ++ "]"
  -- otherwise, we just update the bounds
  | otherwise = Right vState { low_b = Just lb, upp_b = Just ub }

updateValues vState _ = Right vState

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
extractRootFactors _ = Nothing

-- | Applies an "interesting" constraint to update variable states.
analyzeConstraint :: Constraint -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)

-- Rule 4a from Ecne
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
                     let changed = values xState /= values updatedState -- if changes, need to re-queue constraints
                         updatedMap = if changed then Map.insert xID updatedState varStates else varStates
                     in Right (changed, updatedMap)
                   Left errMsg -> Left errMsg
  | otherwise = Right (False, varStates)

-- ROOT Rule from PICUS paper
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
                                    let changed = values xState /= values updatedState
                                        updatedMap = if changed then Map.insert xID updatedState varStates else varStates
                                    in Right (changed, updatedMap)
                                Left errMsg -> Left errMsg
                        Nothing -> Left "Variable state not found in varStates"
                Nothing -> Left "Variable name not found in nameToID"
        Nothing -> Right (False, varStates)  -- not a ROOT constraint

analyzeConstraint _ _ varStates = Right (False, varStates)  -- TODO: Handle other constraints

analyzeConstraints :: Map Int Constraint -> Map String Int -> Map Int [Int] -> Map Int VariableState -> Map Int VariableState
analyzeConstraints constraints nameToID varToConstraints = loop (initializeQueue (Map.elems constraints))
  where
    loop :: Seq Int -> Map Int VariableState -> Map Int VariableState
    loop queue vStates =
      case viewl queue of
        Seq.EmptyL -> vStates
        cId :< restQueue ->
          case Map.lookup cId constraints of
            Just constraint ->
              case analyzeConstraint constraint nameToID vStates of
                Right (changed, newVarStates) ->
                  let newQueue = if changed
                            -- re-queue all constraints that contain the affected variables
                             then let affectedVars = collectVarsFromConstraint Map.empty constraint
                                  in foldl (|>) restQueue (concatMap (\v -> Map.findWithDefault [] v varToConstraints) affectedVars)
                             else restQueue
                  in loop newQueue newVarStates           
                Left errMsg -> error errMsg
            Nothing -> loop restQueue vStates

--------------------------
-- 6) Main Analysis
--------------------------

analyzeProgram :: Program -> Map Int VariableState
analyzeProgram (Program inputs compVars constrVars _ constraints) =
  let nameToID = buildVarNameToIDMap (inputs ++ compVars ++ constrVars)
      varStates = initializeVarStates (inputs ++ compVars ++ constrVars)
      varToConstraints = buildVarToConstraints nameToID constraints
      constraintMap = Map.fromList [(getConstraintID c, c) | c <- constraints]
  in analyzeConstraints constraintMap nameToID varToConstraints varStates