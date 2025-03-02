{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Analysis.Analysis (constantProp, isFullyConstrained) where

import Prelude hiding (iterate)
import qualified Data.Map as Map
import Data.TypeLevel.HMap
import Lattice.ConstantPropagationLattice
import Lattice.Class (Joinable(join))
import Data.Maybe (fromMaybe)
import Syntax.AST
import Domain.Domain

----------------------------------
-- 1) Helper Functions
----------------------------------

-- | Extracts the CP Integer stored at our IntKey from a CirCVal.
-- It uses 'mapList' to iterate over the stored key/value pairs.
-- We pattern-match on the key (using 'SIntKey') to select the value corresponding
-- to the integer constant.
selectCP :: CirCVal -> CP Integer
selectCP (CirCVal hmap) =
  case mapList (\s assoc -> case s of
                              SIntKey -> assoc
                              _       -> Top) hmap of
    [cpVal] -> cpVal
    _       -> Top

-- | Wraps a CP Integer back into a CirCVal.
-- It creates an HMap containing a single field using the 'singleton' function.
injectCPInt :: CP Integer -> CirCVal
injectCPInt = CirCVal . singleton @IntKey

-- | Top represented as a CirC Value.
topCirC :: CirCVal
topCirC = injectCPInt Top

----------------------------------
-- 2) Environment Initialization
----------------------------------

-- | Creates the initial environment for constant propagation.
-- It collects all variable bindings from the program (inputs, computationVars, constraintVars)
-- and maps each variable name to an abstract value representing "unknown" (Top) for the CP lattice.
-- We use 'injectInt Top' to wrap the Top value into a 'CirCVal'.
initEnv :: Program -> Env
initEnv prog = 
    let allBinds = inputs prog ++ computationVars prog ++ constraintVars prog
        initBinding (Binding n _) = (n, topCirC)
    in Map.fromList (Prelude.map initBinding allBinds)   

----------------------------------
-- 3) Expression Evaluation
----------------------------------

-- | Evaluates an 'Expression'.
-- For a variable, it looks up its abstract value in the environment (defaulting to "unknown").
-- For an integer literal, it uses the 'num' injection function.
-- For addition and multiplication, it recursively evaluates the subexpressions,
-- extracts their CP Integer values using 'selectCP', and if both are constants, computes the
-- result. Otherwise, it returns Top. The result is wrapped back into a 'CirCVal' using 'injectInt'.
evalCP :: Env -> Expression -> CirCVal
evalCP env exp = case exp of
    Var x -> fromMaybe topCirC (Map.lookup x env)
    Int i -> num i    
    Add e1 e2 -> 
        let v1 = evalCP env e1
            v2 = evalCP env e2
            cp1 = selectCP v1
            cp2 = selectCP v2
            cpSum = case (cp1, cp2) of
                (Constant a, Constant b) -> Constant (a + b)
                _                        -> Top
        in injectCPInt cpSum 
    Mul e1 e2 -> 
        let v1 = evalCP env e1
            v2 = evalCP env e2
            cp1 = selectCP v1
            cp2 = selectCP v2
            cpProd = case (cp1, cp2) of
                (Constant a, Constant b) -> Constant (a * b)
                _                        -> Top
        in injectCPInt cpProd              

----------------------------------
-- 4) Constraint Application
----------------------------------

-- | Applies a constraint to update the environment.
-- For the moment, we assume the left-hand side of the equality is a variable.
-- We then:
-- 1. Retrieve the current abstract value for that variable from the environment.
-- 2. Evaluate the right-hand side expression.
-- 3. If the current value is Top, we simply update it with the evaluated right-hand side.
--    Otherwise, we join the current CP value with the CP value from the right-hand side.
-- 4. The resulting unified CP value is wrapped back into a 'CirCVal' and used to update the environment.
applyCPConstraint :: Env -> Constraint -> Env
applyCPConstraint env (Eq (Var x) e2) =
  let current = Map.findWithDefault topCirC x env
      v2      = evalCP env e2
      newVal  = case selectCP current of
                  Top -> v2
                  Constant _ ->
                    let unified = join (selectCP current) (selectCP v2)
                    in injectCPInt unified
  in Map.insert x newVal env

----------------------------------
-- 5) Fixpoint Iteration
----------------------------------

-- | Iterates over all constraints repeatedly until the environment no longer changes.
propagateCP :: [Constraint] -> Env -> Env
propagateCP cs env =
    let env' = foldl applyCPConstraint env cs
    in if env == env' then env else propagateCP cs env'

----------------------------------
-- 6) Top-Level Analysis
----------------------------------

-- | Runs constant propagation on the constraints of a given 'Program'.
-- It initializes the environment from the Program's bindings, then propagates the constraints
-- until a fixpoint is reached, and returns the final environment.
constantProp :: Program -> Env
constantProp prog =
    let env0 = initEnv prog
        finalEnv = propagateCP (constraints prog) env0
    in finalEnv