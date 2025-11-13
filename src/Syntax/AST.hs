
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Syntax.AST (Program (..), Binding (..), Sort (..), Expression (..), Constraint (..), Tag (..)) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

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
  deriving (Show, Eq, Generic, NFData)

-- | A binding contains the name and the sort.
data Binding = Binding
  { vid :: Int,
    name :: String,
    sort :: Sort,
    tag :: Maybe Tag
  }
  deriving (Show, Eq, Generic, NFData)

data Tag 
  = SimpleTag String       -- e.g., "binary", "powerof2"
  | MaxBitsTag Integer     -- e.g., (maxbits 5)
  | MaxValTag Integer      -- e.g., (maxvalue 10)
  | MinValTag Integer      -- e.g., (minvalue 5) 
  | MaxAbsTag Integer      -- e.g., (max_abs 10)
  | MaxBitsAbsTag Integer  -- e.g., (maxbit_abs 5)
  deriving (Show, Eq, Generic, NFData)

-- | The sorts we are interested in. Currently, only field elements are included,
--   as they are the primary type supported by Circom.
data Sort
  = FieldMod Integer 
  | Bool 
  | BitVector Integer
  | ArraySort Sort Integer -- arrays have an element sort and size
  deriving (Show, Eq, Generic, NFData)

-- | Expressions: variables, field elements, and arithmetic operations.
data Expression
  = Var String
  | Int Integer
  | FieldConst Integer Integer
  | BoolLit Bool
  | Add Expression Expression
  | Sub Expression Expression
    -- exponentation (**) => multiplication in CirC, so we support exponentation
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

    -- CirC handles a shift like `a << 2` on a variable `a` by first extracting the bits that will remain after the shift. 
    -- Then, it concatenates the extracted bits with a literal value. 
    -- For example, `(extract 5 0) a` takes the lower 6 bits (bits 0 through 5) of `a`, so that the top 2 bits are discarded. 
    -- Then, it concatenates these extracted 6 bits with a literal 2-bit zero (#b00) using concat. 
    -- This places the extracted bits in the higher positions and the two zeros in the lower positions.
    -- In order to support shifts in our AST, we add BvExtract, BvConcat, and BvLit as expression types below.
    -- BvExtract e high low: extract bits [high : low] from expression e
  | BvExtract Expression Integer Integer
    -- BvConcat e1 e2: concatenates bit-vectors e1 and e2
  | BvConcat Expression Expression
    -- BvLit val width: a literal bit-vector of width bits, representing val
  | BvLit Integer Integer

    -- XOR operation
  | BvXor Expression Expression

    -- bv2pf modulus expr: converts bitvector (bv) `expr` to prime field element (pf) mod `modulus`
    -- This operation is needed in CirC for type compatibility, particularly
    -- when using a bitvector value as an index for an array that 
    -- expects prime field indices. The operation interprets the bitvector as an 
    -- unsigned integer and then calculates its value modulo the specified prime modulus. 
    -- For example, ((bv2pf 7) #b1101) takes the bitvector 1101 (unsigned integer 13), 
    -- calculates 13 mod 7, resulting in the prime field element 6. This value can then be 
    -- used for indexing in an array that expects prime field indices.
  | Bv2Pf Integer Expression

    -- Represents the reciprocal of an expression
  | PfRecip Expression

    -- Let expressions
  | Let [(String, Expression)] Expression 

    -- Return statements
  | Return [Expression]   

    -- Tuple
  | Tuple [Expression]  

    -- #l arrays
  | ArrayLiteral [Expression] Sort
    -- #a arrays
  | ArraySparseLiteral [(Integer, Expression)] Expression Integer Sort 
    -- (array ...) constructor
  | ArrayConstruct [Expression] Sort  
    -- select(array, index) 
  | ArraySelect Expression Expression  
    -- store(array, index, value)                
  | ArrayStore Expression Expression Expression        
    -- fill(value, sort, size) 
  | ArrayFill Expression Sort Integer 

    -- assignments
  | Assign String Expression
  deriving (Show, Eq, Generic, NFData)

-- | Minimal set of constraints: equality.
data Constraint
  = EqC Int Expression Expression
  | AndC Int [Constraint]
  | OrC Int [Constraint]
  | NotC Int Constraint
  deriving (Show, Eq, Generic, NFData)