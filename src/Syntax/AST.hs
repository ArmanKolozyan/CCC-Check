
module Syntax.AST (Program (..), Binding (..), Sort (..), Expression (..), Constraint (..)) where

-- | An AST representation of a CirC-IR program. It only contains the
--   components relevant for bug detection. Information about parties,
--   commitments, etc. is thus omitted.
data Program = Program
  { inputs :: [Binding],
    computationVars :: [Binding],
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
  | Bool 
  | BitVector Integer
  deriving (Show, Eq)

-- | Minimal set of expressions: variables, field elements (currently handled
-- as integers), and arithmetic.
-- TODO: support more operators from https://zokrates.github.io/language/operators.html
data Expression
  = Var String
  | Int Integer
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Ite Expression Expression Expression
  | Eq Expression Expression
  | Gt Expression Expression
  | Lt Expression Expression
  | Gte Expression Expression
  | Lte Expression Expression
  | And Expression Expression
  | Or Expression Expression
  | Not Expression
  deriving (Show, Eq)

-- | Minimal set of constraints: equality.
data Constraint
  = EqC Expression Expression
  deriving (Show, Eq)

{-
TODO:
- add support for set_default_modulus
- add support for literal field values (such as #f123m456)
- add support for other Circom types (i.e., arrays) and ZoKrates types.
-}