module Syntax.Compiler (compile, parseAndCompile) where

import Syntax.AST
import Control.Monad ((>=>))
import Control.Monad.Except
import Data.Char (isDigit)

import Syntax.Scheme.Parser
import qualified Syntax.Scheme.Parser as SExp
import GHC.Arr (accum)

--------------------------
-- 1) Top-level
--------------------------

type MonadCompile m = (MonadError String m)

-- | Takes a top-level s-expression (SExp) and produces our Program AST.
compile :: MonadCompile m => SExp -> m Program
compile (Atom "computations" _ ::: (Atom _name _ ::: compBody) ::: SNil _) =
  compileComputation compBody
compile sexp =
  throwError $ "Expected (computations (NAME (computation ...))), got: " ++ show sexp

-- | parses and compiles the given IR representation to AST nodes.
parseAndCompile :: String -> Either String Program
parseAndCompile = fmap head . parseSexp >=> compile

--------------------------
-- 2) Computation

-- We are looking for (Atom "computation" _ ::: forms)
-- where 'forms' could be (metadata ...)/(precompute ...)/(declare ...)/...
--------------------------

-- | Compiles the different forms using helper functions, collecting the following:
-- inputs from metadata
-- computationVars and list of expressions from precompute
-- constraintVars and list of constraints from final declare
compileComputation :: MonadCompile m => SExp -> m Program
compileComputation (Atom "computation" _ ::: forms) = do
   (inputs, compVars, compExps, constrVars, constraints) <- compileForms forms
   return $ Program {
    inputs = inputs,
    comptutationVars = compVars,
    constraintVars = constrVars,
    computations = compExps,
    constraints = constraints
   }
compileComputation e = throwError $ "Expected (computation ...), got: " ++ show e

-- | Handles the compilation of forms by calling compileForm for each individual form and folding
-- the results using sfoldlM.
compileForms:: MonadCompile m => SExp -> m ([Binding], [Binding], [Expression], [Binding], [Constraint])
compileForms forms = do
    let initAcc = ([], [], [], [], []) -- initial empty accumulator
    sfoldlM compileForm initAcc forms

-- | Folds over a list of s-expressions in a monadic context.
sfoldlM :: MonadCompile m => (a -> SExp -> m a) -> a -> SExp -> m a
sfoldlM _ acc (SNil _) = return acc
sfoldlM f acc (exp ::: exps) = do
    acc' <- f acc exp
    sfoldlM f acc' exps
sfoldlM _ _ e = throwError $ "malformed list " ++ show e

-- | Compiles a single form by first matching it to check whether it is a 
-- metadata/precompute/declare/... and then gathers the appropriate information from that form. 
compileForm :: MonadCompile m => ([Binding],[Binding],[Expression],[Binding],[Constraint])
  -> SExp
  -> m ([Binding],[Binding],[Expression],[Binding],[Constraint])
compileForm info@(ins, compv, comps, constv, consts) form = case form of
    ex@(Atom "metadata" _ ::: _) -> do 
        newIns <- compileMetadata ex
        pure (ins ++ newIns, compv, comps, constv, consts)
    ex@(Atom "precompute" _ ::: _) -> do
        (vars, exps) <- compilePrecompute ex
        pure (ins, compv ++ vars, comps ++ exps, constv, consts)
    ex@(Atom "declare" _ ::: _) -> do
        (vars, constrs) <- compileDeclare ex
        pure (ins, compv, comps, constv ++ vars, consts ++ constrs)
    _ -> pure info

--------------------------
-- 3) Metadata

-- We currently only look for (inputs (...)) inside (metadata ...).
--------------------------

-- | Compiles metadata forms by extracting input variable bindings.
compileMetadata :: MonadCompile m => SExp -> m [Binding]
compileMetadata (Atom "metadata" _ ::: metaForms) =
    sfoldlM compileMetaForm [] metaForms
compileMetadata e = throwError $ "Invalid (metadata ...): " ++ show e

-- | Compiles a single metadata form by looking for inputs.
compileMetaForm :: MonadCompile m => [Binding] -> SExp -> m [Binding]
compileMetaForm acc form = case form of
    (Atom "inputs" _ ::: vars) -> do
        newBinds <- sfoldlM compileInput [] vars
        pure (acc ++ newBinds)
    _ -> pure acc

-- | Compiles an input declaration by extracting variable name and sort.
compileInput :: MonadCompile m => [Binding] -> SExp -> m [Binding]
compileInput acc form = case form of
    (Atom "return" _ ::: _) -> pure acc  -- ignoring "return" var
    (Atom varName _ ::: sortExp ::: _ ::: SNil _) -> do
        sort <- compileSort sortExp
        pure (acc ++ [Binding varName sort]) 
    _ -> pure acc 

--------------------------
-- 4) Precompute

-- We currently only look for (declare (...)) and expressions inside (precompute ...).
-- optional (declare ...) ... expressions
--------------------------

-- | Compiles a precompute form by extracting declared variables and expressions.
compilePrecompute :: MonadCompile m => SExp -> m ([Binding], [Expression])
compilePrecompute (Atom "precompute" _ ::: preForms) =
    sfoldlM compilePreForm ([], []) preForms
compilePrecompute e = throwError $ "Invalid (precompute ...): " ++ show e

-- | Compiles a single form within a precompute block. It distinguishes between 
-- declarations and body expressions.
compilePreForm :: MonadCompile m => ([Binding], [Expression]) -> SExp -> m ([Binding], [Expression])
compilePreForm (vars, exps) preForm = case preForm of
    ex@(Atom "declare" _ ::: _) -> do
        (bds, _) <- compileDeclare ex -- TODO: separate declares for precompute and verification
        pure (vars ++ bds, exps)
    _ -> do
        preForm_compiled <- compileExp preForm
        pure (vars, exps ++ [preForm_compiled])    

--------------------------
-- 5) Declare

-- We currently only look for variables and constraints inside the final (declare ...).
-- (declare ( (x sort) (y sort) ...) constraints...)
--------------------------

-- | Compiles a declare block by extracting variable declarations and constraints.
compileDeclare :: MonadCompile m => SExp -> m ([Binding], [Constraint])
compileDeclare (Atom "declare" _ ::: declForms) =
    sfoldlM compileDeclForm ([], []) declForms
compileDeclare e =
    throwError $ "Invalid (declare ...): " ++ show e

-- | Compiles an individual declaration. It checks whether a variable is defined
-- or a constraint is added.
compileDeclForm :: MonadCompile m => ([Binding], [Constraint]) -> SExp -> m ([Binding], [Constraint])
compileDeclForm (bds, consts) form = case form of
    -- (temporary) variable
    (Atom varName _ ::: sortExp ::: SNil _) -> do
        sort <- compileSort sortExp
        pure (bds ++ [Binding varName sort], consts)   
    -- constraint
    (Atom "=" _ ::: lhs ::: rhs ::: SNil _) -> do 
        lhs_compiled <- compileExp lhs
        rhs_compiled <- compileExp rhs   
        pure (bds, consts ++ [Eq lhs_compiled rhs_compiled])
    _ -> pure (bds, consts)

--------------------------
-- 6) Expression

-- Variables, field elements (currently handled as integers), and arithmetic.
--------------------------        

-- | Compiles a single expression.
compileExp :: MonadCompile m => SExp -> m Expression
compileExp (Num i _) = pure (Int i)
compileExp (Atom name _) =
    if all isDigit name && not (null name)
        then pure (Int (read name))
        else pure (Var name)
compileExp (Atom "+" _ ::: e1 ::: e2 ::: SNil _) = do
    lhs_compiled <- compileExp e1
    rhs_compiled <- compileExp e2
    pure (Add lhs_compiled rhs_compiled)
compileExp (Atom "*" _ ::: e1 ::: e2 ::: SNil _) = do
    lhs_compiled <- compileExp e1
    rhs_compiled <- compileExp e2
    pure (Mul lhs_compiled rhs_compiled)
-- compileExp (Atom "tuple" _ ::: rest) = do TODO: support tuples
compileExp e = throwError $ "Unsupported expression: " ++ show e

--------------------------
-- 7) Sort

-- Currently only fieldMods (mod ...) are supported.
--------------------------   

-- | Compiles a single sort.
compileSort :: MonadCompile m => SExp -> m Sort
compileSort (Atom "mod" _ ::: Num n _ ::: SNil _) =
  pure (FieldMod n)
compileSort e = throwError $ "Unsupported sort: " ++ show e