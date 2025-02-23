{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Syntax.AST (Program (..), Binding (..), Sort (..), Expression (..), Constraint (..)) where

-- | An AST representation of a CirC-IR program. It only contains the
--   components relevant for bug detection. Information about parties,
--   commitments, etc. is thus omitted.
data Program = Program
  { inputs :: [Binding],
    comptutationVars :: [Binding],
    constraintVars :: [Binding],
    computations :: [Expression],
    constraints :: [Constraint]
  }
  deriving (Show, Eq)

-- | A binding contains the name and the sort.
data Binding = Binding
  { name :: String,
    sort :: Sort
  }
  deriving (Show, Eq)

-- | The sorts we are interested in. Currently, only field elements are included,
--   as they are the primary type supported by Circom.
data Sort
  = FieldMod Integer
  deriving (Show, Eq)

-- | Minimal set of expressions: variables, field elements (currently handled
-- as integers), and arithmetic.
data Expression
  = Var String
  | Int Integer
  | Add Expression Expression
  | Mul Expression Expression
  deriving (Show, Eq)

-- | Minimal set of constraints: equality.
data Constraint
  = Eq Expression Expression
  deriving (Show, Eq)

{-
TODO:
- add support for set_default_modulus
- add support for literal field values (such as #f123m456)
- add support for other Circom types (i.e., arrays) and ZoKrates types.
-}
