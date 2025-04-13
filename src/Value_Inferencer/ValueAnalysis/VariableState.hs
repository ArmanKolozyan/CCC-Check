module ValueAnalysis.VariableState where


import ValueAnalysis.ValueDomain (ValueDomain, defaultValueDomain)

-- | Tracks the state of each variable.
newtype VariableState = VariableState {domain :: ValueDomain} deriving (Eq, Show)

-- | Initial VariableState (no restrictions at the beginning).
initVarState :: VariableState
initVarState = VariableState { domain = defaultValueDomain }