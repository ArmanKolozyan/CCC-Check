
module Syntax.AST (Program (..), Binding (..), Sort (..), Expression (..), Constraint (..)) where

-- | An AST representation of a CirC-IR program. It only contains the
--   components relevant for bug detection. Information about parties,
--   commitments, etc. is thus omitted.
data Program = Program
  { inputs :: [Binding],
    computationVars :: [Binding],
    constraintVars :: [Binding],
    computations :: [Expression],
    pfRecipExpressions :: [Expression], -- to collect all denominators (exprs that cannot be 0)
    returnVars         :: [Binding],
    constraints :: [Constraint]
  }
  deriving (Show, Eq)

-- | A binding contains the name and the sort.
data Binding = Binding
  { vid :: Int,
    name :: String,
    sort :: Sort
  }
  deriving (Show, Eq)

-- | The sorts we are interested in. Currently, only field elements are included,
--   as they are the primary type supported by Circom.
data Sort
  = FieldMod Integer 
  | Bool 
  | BitVector Integer
  | NonZero -- TODO: op een betere manier encoderen/aangeven
  deriving (Show, Eq)

-- | Minimal set of expressions: variables, field elements (currently handled
-- as integers), and arithmetic.
-- TODO: support more operators from https://zokrates.github.io/language/operators.html
data Expression
  = Var String
  | Int Integer
  | FieldConst Integer Integer
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  | Ite Expression Expression Expression
  | Eq Expression Expression
  | Gt Expression Expression
  | Lt Expression Expression
  | Gte Expression Expression
  | Lte Expression Expression
  | And [Expression]
  | Or [Expression]
  | Not Expression
    -- BvExtract e high low: extract bits [high : low] from expression e
  | BvExtract Expression Integer Integer
    -- BvConcat e1 e2: concatenates bit-vectors e1 and e2
  | BvConcat Expression Expression
    -- BvLit val width: a literal bit-vector of width bits, representing val
  | BvLit Integer Integer
    -- Represents the reciprocal of an expression
  | PfRecip Expression 
    -- Let expressions
  | Let [(String, Expression)] Expression 
    -- Return statements
  | Return [Expression]     
  deriving (Show, Eq)

-- | Minimal set of constraints: equality.
data Constraint
  = EqC Int Expression Expression
  | AndC Int [Constraint]
  | OrC Int [Constraint]
  | NotC Int Constraint
  deriving (Show, Eq)

{-
TODO:
- add support for set_default_modulus
- add support for literal field values (such as #f123m456)
- add support for other Circom types (i.e., arrays) and ZoKrates types.
-}