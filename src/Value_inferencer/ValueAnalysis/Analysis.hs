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
updateValues :: VariableState -> Set Integer -> Either String VariableState
updateValues vState newVals
  -- if values are already known, we check if newVals is a subset
  | not (Set.null (values vState)) =
      if Set.isSubsetOf newVals (values vState)
      then Right vState { values = newVals }  -- Safe update
      else Left $ "Inconsistent value inference! New values " ++ show newVals
                ++ " are not a subset of existing " ++ show (values vState)

  -- if no explicit values exist, we use (possible) bounds to infer potential values
  | otherwise =
      let lowerBound = fromMaybe (minimum newVals) (low_b vState)
          upperBound = fromMaybe (maximum newVals) (upp_b vState)
          inferredVals = Set.fromList [lowerBound .. upperBound]
      in if Set.isSubsetOf newVals inferredVals
         then Right vState { values = newVals }  -- Update safely
         else Left $ "Value inference contradicts bounds! New values: "
                  ++ show newVals ++ " do not fit in inferred range "
                  ++ show (Set.toList inferredVals)

getVarID = Map.lookup

-- | Recursively infers possible values of an expression
inferValues :: Expression -> Map String Int -> Map Int VariableState -> Set Integer
inferValues (Int c) _ _ = Set.singleton c
inferValues (Var xName) nameToID varStates = 
  case getVarID xName nameToID of
        Just varID -> maybe Set.empty values (Map.lookup varID varStates)
        Nothing    -> Set.empty
inferValues (Add e1 e2) nameToID varStates =
  Set.fromList [v1 + v2 | v1 <- Set.toList (inferValues e1 nameToID varStates),
                          v2 <- Set.toList (inferValues e2 nameToID varStates)]
inferValues (Sub e1 e2) nameToID varStates =
  Set.fromList [v1 - v2 | v1 <- Set.toList (inferValues e1 nameToID varStates),
                          v2 <- Set.toList (inferValues e2 nameToID varStates)]
inferValues (Mul e1 e2) nameToID varStates =
  Set.fromList [v1 * v2 | v1 <- Set.toList (inferValues e1 nameToID varStates),
                          v2 <- Set.toList (inferValues e2 nameToID varStates)]
inferValues _ _ _ = Set.empty -- TODO: Handle other cases properly

-- Helper function: Computes modular inverse
modularInverse :: Integer -> Integer
modularInverse c = if c /= 0 then 1 `div` c else error "Division by zero"
-- TODO: Implement modular inverse properly for field arithmetic.

-- | Applies an "interesting" constraint to update variable states.
analyzeConstraint :: Constraint -> Map String Int -> Map Int VariableState -> Either String (Bool, Map Int VariableState)

-- Rule 4a from Ecne
analyzeConstraint (EqC _ (Var xName) (Var yName)) nameToID varStates =
  case (Map.lookup xName nameToID, Map.lookup yName nameToID) of
    (Just x, Just y) -> case (Map.lookup x varStates, Map.lookup y varStates) of
      (Just vx, Just vy) ->
        let newValues = Set.union (values vx) (values vy)
            changed = newValues /= values vx || newValues /= values vy
            newVarStates = if changed
                           then Map.insert x (vx { values = newValues }) $
                                Map.insert y (vy { values = newValues }) varStates
                           else varStates
        in Right (changed, newVarStates)
      _ -> Right (False, varStates)
    _ -> Left "Variable name not found in nameToID map"

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