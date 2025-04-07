module ValueAnalysis.VariableState where

import Data.Set (Set)
import qualified Data.Set as Set

-- | Tracks the state of each variable.
data VariableState = VariableState
  {
    values   :: Set Integer,
    low_b    :: Maybe Integer,
    upp_b    :: Maybe Integer,
    nonZero  :: Bool
  }
  deriving (Eq, Show)

-- | Initial VariableState (no restrictions at the beginning).
initVarState :: VariableState
initVarState = VariableState
  { values = Set.empty,
    low_b = Nothing,
    upp_b = Nothing,
    nonZero = False
  }  