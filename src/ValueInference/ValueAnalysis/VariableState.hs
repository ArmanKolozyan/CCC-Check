{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ValueAnalysis.VariableState where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import ValueAnalysis.ValueDomain (ValueDomain(..), defaultValueDomain)
import Syntax.AST
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- | Tracks the state of each variable.
newtype VariableState = VariableState {domain :: ValueDomain} deriving (Eq, Show, Generic, NFData)

-- | Initializes the state for a single variable based on its binding.
initVarState :: Binding -> VariableState
initVarState binding =
  let initialDomain = case sort binding of
        ArraySort _ size -> ArrayDomain Map.empty defaultValueDomain size -- array
        _                -> defaultValueDomain -- other types
  in VariableState { domain = initialDomain }

-- | Initializes the state for a single variable to default.
initVarStateDefault :: VariableState
initVarStateDefault = VariableState { domain = defaultValueDomain }

-- | Builds a map from variable IDs to their initial state.
initializeVarStates :: [Binding] -> IntMap VariableState
initializeVarStates vars = IntMap.fromList [(vid v, initVarState v) | v <- vars]

-- | Builds a map from variable names to their IDs for lookup.
buildVarNameToIDMap :: [Binding] -> Map.Map String Int
buildVarNameToIDMap vars = Map.fromList [(name v, vid v) | v <- vars]

-- | Lookup variable ID by name.
lookupVarID :: String -> Map.Map String Int -> Either String Int
lookupVarID name nameToID =
  case Map.lookup name nameToID of
    Just vID -> Right vID
    Nothing  -> Left $ "Variable name not found in nameToID map: " ++ name

-- | Lookup variable state by ID.
lookupVarState :: Int -> IntMap VariableState -> Either String VariableState
lookupVarState vID varStates =
  case IntMap.lookup vID varStates of
    Just state -> Right state
    Nothing    -> Left $ "Variable state not found in varStates for ID: " ++ show vID

-- | Lookup variable state by name.
lookupVarStateByName :: String -> Map.Map String Int -> IntMap VariableState -> Either String VariableState
lookupVarStateByName name nameToID varStates = do
  vID <- lookupVarID name nameToID
  lookupVarState vID varStates
