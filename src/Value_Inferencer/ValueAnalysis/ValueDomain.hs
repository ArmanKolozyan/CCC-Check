module ValueAnalysis.ValueDomain where

import Data.Set (Set)

-- | Represents values of variables.
data ValueDomain
  = KnownValues (Set Integer)                 -- explicitly known values
  | BoundedValues (Maybe Integer) (Maybe Integer)  -- lower and upper bounds
  deriving (Eq, Show)
