{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ValueAnalysis.VariableState where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import ValueAnalysis.ValueDomain (ValueDomain(..), defaultValueDomain)
import Syntax.AST
import Data.Map
import qualified Data.Map as Map

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
initializeVarStates :: [Binding] -> Map Int VariableState
initializeVarStates vars = Map.fromList [(vid v, initVarState v) | v <- vars]

-- | Builds a map from variable names to their IDs for lookup.
buildVarNameToIDMap :: [Binding] -> Map String Int
buildVarNameToIDMap vars = Map.fromList [(name v, vid v) | v <- vars]

-- | Lookup variable ID by name.
lookupVarID :: String -> Map String Int -> Either String Int
lookupVarID name nameToID =
  case Map.lookup name nameToID of
    Just vID -> Right vID
    Nothing  -> Left $ "Variable name not found in nameToID map: " ++ name

-- | Lookup variable state by ID.
lookupVarState :: Int -> Map Int VariableState -> Either String VariableState
lookupVarState vID varStates =
  case Map.lookup vID varStates of
    Just state -> Right state
    Nothing    -> Left $ "Variable state not found in varStates for ID: " ++ show vID

-- | Lookup variable state by name.
lookupVarStateByName :: String -> Map String Int -> Map Int VariableState -> Either String VariableState
lookupVarStateByName name nameToID varStates = do
  vID <- lookupVarID name nameToID
  lookupVarState vID varStates