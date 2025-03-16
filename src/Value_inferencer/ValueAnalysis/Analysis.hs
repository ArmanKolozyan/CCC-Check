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
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq


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


-- | Initializing VariableState (no restrictions at the beginning).
initVarState :: VariableState
initVarState = VariableState
  { values = Set.empty,
    low_b = Nothing,
    upp_b = Nothing
  }  

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

--------------------------
-- 3) Mapping Variables to Constraints
--------------------------

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
-- 5) Main Analysis
--------------------------

analyzeProgram :: Program -> (Map Int VariableState, Map Int [Int])
analyzeProgram (Program inputs compVars constrVars _ constraints) =
  let nameToID = buildVarNameToIDMap (inputs ++ compVars ++ constrVars)
      varStates = initializeVarStates (inputs ++ compVars ++ constrVars)
      varToConstraints = buildVarToConstraints nameToID constraints
      queue = initializeQueue constraints
  in (varStates, varToConstraints)