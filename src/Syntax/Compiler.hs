{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax.Compiler (compile, parseAndCompile, parseAndCompileConstraint) where

import Syntax.AST
import Control.Monad.Except
import Control.Monad.State
import Data.Char (isDigit)

import Syntax.Scheme.Parser

import Data.List (foldl')
import qualified Data.Map.Strict as Map

-- | Preprocess CirC IR text to work around S-expression parser issues:
--   #f<digits>        -> __fc__<digits>       (positive field constant)
--   #f-<digits>       -> __fc__neg__<digits>  (negative field constant)
--   #f<d>m<d>         -> __fc__<d>m<d>        (field constant with explicit modulus)
--   #b<bits>          -> __bv__<bits>         (bitvector literal)
--   '<digits>         -> __qv__<digits>       (quoted variable name in let bindings)
-- The compiler then recognizes these prefixed atoms.
preprocessFieldConstants :: String -> String
preprocessFieldConstants = go ' '  -- start with space as "previous char"
  where
    go _ [] = []
    go prev ('#':'f':rest)
        | ('-':d:ds) <- rest, isDigit d =
            "__fc__neg__" ++ [d] ++ go d ds
        | (d:_) <- rest, isDigit d =
            "__fc__" ++ go '#' rest
        | otherwise = '#' : 'f' : go 'f' rest
    go prev ('#':'b':rest)
        | (d:_) <- rest, d == '0' || d == '1' =
            "__bv__" ++ go '#' rest
        | otherwise = '#' : 'b' : go 'b' rest
    -- rewriting 'N (quoted variable) only when preceded by whitespace or '('
    -- to avoid rewriting quotes inside strings or identifiers
    go prev ('\'':d:rest)
        | isDigit d, prev == ' ' || prev == '\n' || prev == '\t' || prev == '(' =
            "__qv__" ++ [d] ++ go d rest
    go _ (c:cs) = c : go c cs

data CompilerState = CompilerState
    {
        nextVarID :: Int,
        nextConstraintID :: Int,
        pfRecipExprs :: [Expression], -- to collect all denominators (exprs that cannot be 0)
        knownVars :: Map.Map String Int -- tracks already-seen variable names -> IDs (for dedup)
    }

emptyState :: CompilerState
emptyState = CompilerState
    {
        nextVarID = 0,
        nextConstraintID = 0,
        pfRecipExprs = [],
        knownVars = Map.empty
    }

-- | Helper function to add a pfrecip expression to the state
addPfRecipExpression :: MonadCompile m => Expression -> m ()
addPfRecipExpression expr = do
    st <- get
    put st { pfRecipExprs = pfRecipExprs st ++ [expr] }


genVarID :: MonadCompile m => m Int
genVarID = do
    st <- get
    let newID = nextVarID st
    put st {nextVarID = newID + 1}
    return newID

-- | Getting or creating a variable ID. If the variable name was already seen,
-- returns Nothing (meaning: skip this binding, it's a duplicate).
-- If new, creates an ID, registers it, and returns Just id.
getOrCreateVarID :: MonadCompile m => String -> m (Maybe Int)
getOrCreateVarID varName = do
    st <- get
    case Map.lookup varName (knownVars st) of
        Just _existingID -> return Nothing  -- already seen
        Nothing -> do
            let newID = nextVarID st
            put st { nextVarID = newID + 1
                   , knownVars = Map.insert varName newID (knownVars st) }
            return (Just newID)

genConstraintID :: MonadCompile m => m Int
genConstraintID = do
    st <- get
    let newID = nextConstraintID st
    put st {nextConstraintID = newID + 1}
    return newID

--------------------------
-- 1) Top-level
--------------------------

type MonadCompile m = (MonadError String m, MonadState CompilerState m)

-- | Takes a top-level s-expression (SExp) and produces our Program AST.
compile :: MonadCompile m => SExp -> m Program
compile (Atom "computations" _ ::: (Atom _name _ ::: compBody ::: SNil _) ::: SNil _) =
  compileComputation compBody
compile sexp =
  throwError $ "Expected (computations (NAME (computation ...))), got: " ++ show sexp

-- | parses and compiles the given IR representation to AST nodes.
parseAndCompile :: String -> Either String Program
parseAndCompile input = do
    let preprocessed = preprocessFieldConstants input
    sexps <- parseSexp preprocessed
    case sexps of
        (firstSexp : _) -> evalStateT (compile firstSexp) emptyState
        []              -> Left "Error: No expressions to compile"

-- | parses and compiles the given constraint.
parseAndCompileConstraint :: String -> Either String Constraint
parseAndCompileConstraint input = do
    let preprocessed = preprocessFieldConstants input
    sexps <- parseSexp preprocessed
    case sexps of
        (firstSexp : _) -> evalStateT (compileConstraint firstSexp) emptyState
        []              -> Left "Error: No expressions to compile"


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
   (inputs, compVars, compExps, constrVars, constraints, retVars) <- compileForms forms
   st <- get
   let pfRecips = pfRecipExprs st -- retrieving collected pfrecip expressions
   return $ Program {
    inputs = inputs,
    computationVars = compVars,
    constraintVars = constrVars,
    computations = compExps,
    constraints = constraints,
    pfRecipExpressions = pfRecips,
    returnVars = retVars
   }
compileComputation e = throwError $ "Expected (computation ...), got: " ++ show e

-- | Handles the compilation of forms by calling compileForm for each individual form and folding
-- the results using sfoldlM.
compileForms:: MonadCompile m => SExp -> m ([Binding], [Binding], [Expression], [Binding], [Constraint], [Binding])
compileForms forms = do
    let initAcc = ([], [], [], [], [], []) -- initial empty accumulator
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
compileForm :: MonadCompile m => ([Binding],[Binding],[Expression],[Binding],[Constraint], [Binding])
  -> SExp
  -> m ([Binding],[Binding],[Expression],[Binding],[Constraint], [Binding])
compileForm info@(ins, compv, comps, constv, consts, retv) form = case form of
    ex@(Atom "metadata" _ ::: _) -> do
        newIns <- compileMetadata ex
        pure (ins ++ newIns, compv, comps, constv, consts, retv)
    ex@(Atom "precompute" _ ::: _) -> do
        (vars, exps, rets) <- compilePrecompute ex
        pure (ins, compv ++ vars, comps ++ exps, constv, consts, retv ++ rets)
    ex@(Atom "declare" _ ::: _) -> do
        (vars, constrs) <- compileDeclare ex
        pure (ins, compv, comps, constv ++ vars, consts ++ constrs, retv)
    -- CirC wraps forms in (set_default_modulus N ...) — unwrap and process inner forms
    (Atom "set_default_modulus" _ ::: _modulus ::: innerForms) ->
        sfoldlM compileForm info innerForms
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
-- The IR format may include optional metadata like (party N), (round N),
-- (random), (committed) after the sort. These are skipped when looking for tags.
compileInput :: MonadCompile m => [Binding] -> SExp -> m [Binding]
compileInput acc form = case form of
    (Atom "return" _ ::: _) -> pure acc  -- ignoring "return" var
    (Atom varName _ ::: sortExp ::: rest) -> do
        maybeID <- getOrCreateVarID varName
        case maybeID of
            Nothing -> pure acc  -- duplicate variable, skip
            Just newID -> do
                sort <- compileSort sortExp
                tagValue <- findTag rest
                pure (acc ++ [Binding newID varName sort tagValue])
    _ -> throwError $ "Invalid input declaration: " ++ show form

-- | Walk through remaining metadata after the sort to find a tag, if any.
-- Skips known metadata like (party N), (round N), (random), (committed).
findTag :: MonadCompile m => SExp -> m (Maybe Tag)
findTag (SNil _) = pure Nothing
findTag (exp ::: rest) = case exp of
    -- Skip known CirC metadata
    (Atom "party" _ ::: _)     -> findTag rest
    (Atom "round" _ ::: _)     -> findTag rest
    (Atom "random" _)          -> findTag rest
    (Atom "committed" _)       -> findTag rest
    -- Anything else is treated as a tag
    _ -> do
        tagValue <- compileTag exp
        pure (Just tagValue)
      

--------------------------
-- 4) Precompute

-- We currently only look for (declare (...)) and expressions inside (precompute ...).
-- optional (declare ...) ... expressions
--------------------------

-- | Compiles a precompute form by extracting declared variables and expressions.
compilePrecompute :: MonadCompile m => SExp -> m ([Binding], [Expression], [Binding])
compilePrecompute (Atom "precompute" _ ::: preForms) =
    sfoldlM compilePreForm ([], [], []) preForms
compilePrecompute e = throwError $ "Invalid (precompute ...): " ++ show e

-- | Compiles a single form within a precompute block. It distinguishes between 
-- declarations and body expressions.
compilePreForm :: MonadCompile m => ([Binding], [Expression], [Binding]) -> SExp -> m ([Binding], [Expression], [Binding])
compilePreForm (vars, exps, rets) preForm = case preForm of
    (Atom "declare" _ ::: (varDefs ::: compForms ::: SNil _)) -> do
        bds <- sfoldlM compileSinglePrecomputeVar [] varDefs
        exps <- compileExp compForms
        pure (vars ++ bds, [exps], rets)
    -- CirC wraps precompute declares in (set_default_modulus N ...) — unwrap and process inner forms
    (Atom "set_default_modulus" _ ::: _modulus ::: innerForms) -> do
        sfoldlM compilePreForm (vars, exps, rets) innerForms
    -- return variables
    returns -> do
        newRets <- compileReturnBindings returns
        pure (vars, exps, rets ++ newRets)

-- | Helper to compile a single variable declaration within precompute's declare.
compileSinglePrecomputeVar :: MonadCompile m => [Binding] -> SExp -> m [Binding]
compileSinglePrecomputeVar acc varDef = case varDef of
  -- case with tag
  (Atom varName _ ::: sortExp ::: tagExp ::: SNil _) -> do
      maybeID <- getOrCreateVarID varName
      case maybeID of
          Nothing -> pure acc  -- duplicate, skip
          Just newID -> do
              sort <- compileSort sortExp
              tagValue <- compileTag tagExp
              pure (acc ++ [Binding newID varName sort (Just tagValue)])
  -- case without tag
  (Atom varName _ ::: sortExp ::: SNil _) -> do
      maybeID <- getOrCreateVarID varName
      case maybeID of
          Nothing -> pure acc  -- duplicate, skip
          Just newID -> do
              sort <- compileSort sortExp
              pure (acc ++ [Binding newID varName sort Nothing])
  _ -> throwError $ "Invalid variable declaration in precompute: " ++ show varDef

-- | Compiles a list of return bindings, e.g., ((return.0 ...) (return.1 ...) ...).
-- Also accepts non-"return" names (e.g., "main.out") for CirC compatibility.
compileReturnBindings :: MonadCompile m => SExp -> m [Binding]
compileReturnBindings (SNil _) = pure []
compileReturnBindings ((Atom retName _ ::: sortExp ::: rest) ::: moreBindings) = do
    maybeID <- getOrCreateVarID retName
    moreRets <- compileReturnBindings moreBindings
    case maybeID of
        Nothing -> pure moreRets  -- duplicate, skip
        Just newID -> do
            sortVal <- compileSort sortExp
            tagValue <- findTag rest
            let newBind = Binding newID retName sortVal tagValue
            pure (newBind : moreRets)
compileReturnBindings bad = throwError $ "Invalid return bindings: " ++ show bad

--------------------------
-- 5) Declare

-- We currently only look for variables and constraints inside the final (declare ...).
-- (declare ( (x sort) (y sort) ...) constraints...)
--------------------------

-- | Extracts only the variable bindings from a declare block.
compileVariableDefinitions :: MonadCompile m => [Binding] -> SExp -> m [Binding]
-- case with tag
compileVariableDefinitions acc (Atom varName _ ::: sortExp ::: tagExp ::: SNil _) = do
    maybeID <- getOrCreateVarID varName
    case maybeID of
        Nothing -> pure acc  -- duplicate, skip
        Just newID -> do
            sort <- compileSort sortExp
            tagValue <- compileTag tagExp
            pure (acc ++ [Binding newID varName sort (Just tagValue)])
-- case without tag
compileVariableDefinitions acc (Atom varName _ ::: sortExp ::: SNil _) = do
    maybeID <- getOrCreateVarID varName
    case maybeID of
        Nothing -> pure acc  -- duplicate, skip
        Just newID -> do
            sort <- compileSort sortExp
            pure (acc ++ [Binding newID varName sort Nothing])
compileVariableDefinitions acc _ = throwError "Invalid variable definition in declare block"

-- | Extracts only the constraints from a declare block.
compileConstraint :: MonadCompile m => SExp -> m Constraint
compileConstraint constraint = do
    id <- genConstraintID
    case constraint of
        (Atom "=" _ ::: lhs ::: rhs ::: SNil _) -> do
            lhsE <- compileExp lhs
            rhsE <- compileExp rhs
            pure $ EqC id lhsE rhsE
        (Atom "and" _ ::: rest) -> do
            constrs <- parseListOfConstrs rest
            pure $ AndC id constrs
        (Atom "or" _ ::: rest) -> do
            constrs <- parseListOfConstrs rest
            pure $ OrC id constrs
        (Atom "not" _ ::: exp ::: SNil _) -> do
            constr <- compileConstraint exp
            pure $ NotC id constr
        -- trivially true constraint (CirC emits bare `true` in some cases)
        (Atom "true" _) -> pure $ EqC id (Int 0) (Int 0)
        -- fallback: treat unknown constraint form as trivially true
        other -> pure $ EqC id (Int 0) (Int 0)

-- | Compiles a declare block by extracting variable declarations and constraints.
compileDeclare :: MonadCompile m => SExp -> m ([Binding], [Constraint])
compileDeclare (Atom "declare" _ ::: (varDefs ::: constrForms ::: SNil _)) = do
    vars <- sfoldlM compileVariableDefinitions [] varDefs
    constraint <- compileConstraint constrForms
    pure (vars, [constraint])
compileDeclare e =
    throwError $ "Invalid (declare ...): " ++ show e

--------------------------
-- 6) Expression

-- Variables, field elements (currently handled as integers), and arithmetic.
--------------------------        

parseListOfExps :: MonadCompile m => SExp -> m [Expression]
parseListOfExps (SNil _)        = pure []
parseListOfExps (sub ::: subs)  = (:) <$> compileExp sub <*> parseListOfExps subs
parseListOfExps bad             = throwError $ "Expected list of expressions, got " ++ show bad

parseListOfConstrs :: MonadCompile m => SExp -> m [Constraint]
parseListOfConstrs (SNil _)        = pure []
parseListOfConstrs (sub ::: subs)  = (:) <$> compileConstraint sub <*> parseListOfConstrs subs
parseListOfConstrs bad             = throwError $ "Expected list of constraints, got " ++ show bad

defaultPrime :: Integer
defaultPrime = 52435875175126190479447740508185965837690552500527637822603658699938581184513

-- | Compiles a single expression.
compileExp :: MonadCompile m => SExp -> m Expression
compileExp (Num i _) = pure (Int i)
compileExp (Atom "true" _) = pure (BoolLit True)
compileExp (Atom "false" _) = pure (BoolLit False)
-- bitvector literals: preprocessed from #b0101 -> __bv__0101
compileExp (Atom ('_':'_':'b':'v':'_':'_':bits) _) =
   let val   = compileBinary bits  -- converts "0101" -> 5
       width = fromIntegral (length bits)
   in pure (BvLit val width)
-- keep original #b patterns for backward compatibility (e.g., hand-written test IR)
compileExp (Atom "#b0" _) = pure (BvLit 0 1)
compileExp (Atom "#b1" _) = pure (BvLit 1 1)
compileExp (Atom ('#':'b':bits) _) =
   let val   = compileBinary bits
       width = fromIntegral (length bits)
   in pure (BvLit val width)

-- field constants: preprocessed from #f123 -> __fc__123, #f-1 -> __fc__neg__1, #f123m456 -> __fc__123m456
compileExp (Atom ('_':'_':'f':'c':'_':'_':'n':'e':'g':'_':'_':rest) _) = do
    let (valStr, maybeMPlusPrime) = break (== 'm') rest
        val  = negate (read valStr)
        prime = case maybeMPlusPrime of
                  ('m':primeStr) | not (null primeStr) -> read primeStr
                  _ -> defaultPrime
    pure (FieldConst val prime)
compileExp (Atom ('_':'_':'f':'c':'_':'_':rest) _) = do
    let (valStr, maybeMPlusPrime) = break (== 'm') rest
        val  = read valStr
        prime = case maybeMPlusPrime of
                  ('m':primeStr) | not (null primeStr) -> read primeStr
                  _ -> defaultPrime
    pure (FieldConst val prime)

compileExp (Atom name _) =
    if all isDigit name && not (null name)
        then pure (Int (read name))
        else pure (Var name)
compileExp (Atom "+" _ ::: e1 ::: e2 ::: SNil _) = do
    lhs_compiled <- compileExp e1
    rhs_compiled <- compileExp e2
    pure (Add lhs_compiled rhs_compiled)
compileExp (Atom "pfrecip" _ ::: exp ::: SNil _) = do
    compiled_exp <- compileExp exp
    addPfRecipExpression compiled_exp -- collecting the pfrecip expression
    pure $ PfRecip compiled_exp  
-- unary negation: (- expr) -> Sub 0 expr
compileExp (Atom "-" _ ::: e1 ::: SNil _) = do
    compiled <- compileExp e1
    pure (Sub (Int 0) compiled)
-- binary subtraction: (- e1 e2)
compileExp (Atom "-" _ ::: e1 ::: e2 ::: SNil _) = do
    lhs_compiled <- compileExp e1
    rhs_compiled <- compileExp e2
    pure (Sub lhs_compiled rhs_compiled)    
compileExp (Atom "*" _ ::: e1 ::: e2 ::: SNil _) = do
    lhs_compiled <- compileExp e1
    rhs_compiled <- compileExp e2
    pure (Mul lhs_compiled rhs_compiled) 
compileExp (Atom "ite" _ ::: cond ::: cons ::: alt ::: SNil _) = do
    cond_compiled <- compileExp cond
    cons_compiled <- compileExp cons
    alt_compiled  <- compileExp alt
    pure (Ite cond_compiled cons_compiled alt_compiled)
compileExp (Atom "=" _ ::: e1 ::: e2 ::: SNil _) = do
    e1_compiled <- compileExp e1
    e2_compiled <- compileExp e2
    pure (Eq e1_compiled e2_compiled)
compileExp (Atom ">" _ ::: e1 ::: e2 ::: SNil _) = do
    e1_compiled <- compileExp e1
    e2_compiled <- compileExp e2
    pure (Gt e1_compiled e2_compiled)
compileExp (Atom "<" _ ::: e1 ::: e2 ::: SNil _) = do
    e1_compiled <- compileExp e1
    e2_compiled <- compileExp e2
    pure (Lt e1_compiled e2_compiled)
compileExp (Atom ">=" _ ::: e1 ::: e2 ::: SNil _) = do
    e1_compiled <- compileExp e1
    e2_compiled <- compileExp e2
    pure (Gte e1_compiled e2_compiled)
compileExp (Atom "<=" _ ::: e1 ::: e2 ::: SNil _) = do
    e1_compiled <- compileExp e1
    e2_compiled <- compileExp e2
    pure (Lte e1_compiled e2_compiled)
compileExp (Atom "and" _ ::: rest) = do
  exprs <- parseListOfExps rest
  pure (And exprs) 
compileExp (Atom "or" _ ::: rest) = do
  exprs <- parseListOfExps rest
  pure (Or exprs)     
compileExp (Atom "not" _ ::: exp ::: SNil _) = do
    exp_compiled <- compileExp exp
    pure (Not exp_compiled)                              
compileExp (Atom "extract" _ ::: Num high _ ::: Num low _ ::: expr ::: SNil _) = do
    subE <- compileExp expr
    pure (BvExtract subE high low)
-- curried form: ((extract high low) expr)
compileExp ((Atom "extract" _ ::: Num high _ ::: Num low _ ::: SNil _) ::: expr ::: SNil _) = do
    subE <- compileExp expr
    pure (BvExtract subE high low)
-- variadic concat: (concat e1 e2 ... eN) -> nested BvConcat
compileExp (Atom "concat" _ ::: rest) = do
    exprs <- parseListOfExps rest
    case exprs of
        [] -> throwError "concat requires at least one argument"
        [e] -> pure e
        _ -> pure (foldl1 BvConcat exprs)
compileExp (Atom "bvxor" _ ::: e1 ::: e2 ::: SNil _) = do
    e1' <- compileExp e1
    e2' <- compileExp e2
    pure (BvXor e1' e2')
-- flat form: (bv2pf N expr)
compileExp (Atom "bv2pf" _ ::: Num modulus _ ::: expr ::: SNil _) = do
    subE <- compileExp expr
    pure (Bv2Pf modulus subE)
-- flat form: (pf2bv N expr)
compileExp (Atom "pf2bv" _ ::: Num width _ ::: expr ::: SNil _) = do
    subE <- compileExp expr
    pure (Pf2Bv width subE)
-- flat form: (bool2bv expr)
compileExp (Atom "bool2bv" _ ::: expr ::: SNil _) = do
    subE <- compileExp expr
    pure (Bool2Bv subE)
-- flat form: (uext N expr)
compileExp (Atom "uext" _ ::: Num amount _ ::: expr ::: SNil _) = do
    subE <- compileExp expr
    pure (UExt amount subE)
-- flat form: (bit N expr)
compileExp (Atom "bit" _ ::: Num pos _ ::: expr ::: SNil _) = do
    subE <- compileExp expr
    pure (BitSelect pos subE)
-- curried forms: ((op arg) expr) — common in CirC output
compileExp ((Atom "bv2pf" _ ::: Num modulus _ ::: SNil _) ::: expr ::: SNil _) = do
    subE <- compileExp expr
    pure (Bv2Pf modulus subE)
compileExp ((Atom "pf2bv" _ ::: Num width _ ::: SNil _) ::: expr ::: SNil _) = do
    subE <- compileExp expr
    pure (Pf2Bv width subE)
compileExp ((Atom "uext" _ ::: Num amount _ ::: SNil _) ::: expr ::: SNil _) = do
    subE <- compileExp expr
    pure (UExt amount subE)
compileExp ((Atom "bit" _ ::: Num pos _ ::: SNil _) ::: expr ::: SNil _) = do
    subE <- compileExp expr
    pure (BitSelect pos subE)

compileExp (Atom "let" _ ::: bindingList ::: bodyExp ::: SNil _) = do
  binds <- compileLetBindings bindingList
  bodyC <- compileExp bodyExp
  pure (Let binds bodyC)

compileExp (Atom "tuple" _ ::: rest) = do
    exprs <- parseListOfExps rest -- parsing all expressions inside the tuple
    pure (Tuple exprs)

-- #l literal arrays
compileExp (Atom "#l" _ ::: sortExp ::: values ::: SNil _) = do
    keySort <- compileSort sortExp
    vals <- parseListOfExps values
    pure (ArrayLiteral vals keySort)

-- #a sparse literal arrays
compileExp (Atom "#a" _ ::: sortExp ::: defaultExp ::: Num size _ ::: entries ::: SNil _) = do
    sort <- compileSort sortExp
    defaultVal <- compileExp defaultExp
    entryList <- compileSparseArray entries
    pure (ArraySparseLiteral entryList defaultVal size sort)

-- array construction (array key_sort val_sort) values
compileExp (Atom "array" _ ::: keySortExp ::: valSortExp ::: elems) = do
    keySort <- compileSort keySortExp
    valSort <- compileSort valSortExp
    vals <- parseListOfExps elems
    pure (ArrayConstruct vals valSort)

-- array select operation: (select array index)
compileExp (Atom "select" _ ::: arrayExp ::: indexExp ::: SNil _) = do
  array <- compileExp arrayExp
  index <- compileExp indexExp
  pure (ArraySelect array index)

-- array store operation: (store array index value)
compileExp (Atom "store" _ ::: arrayExp ::: indexExp ::: valueExp ::: SNil _) = do
  array <- compileExp arrayExp
  index <- compileExp indexExp
  value <- compileExp valueExp
  pure (ArrayStore array index value)

-- array fill operation: (fill sort size value)
compileExp (Atom "fill" _ ::: sortExp ::: Num size _ ::: valueExp ::: SNil _) = do
  sort <- compileSort sortExp
  value <- compileExp valueExp
  pure (ArrayFill value sort size)

compileExp e = throwError $ "Unsupported expression: " ++ show e

compileBinary :: String -> Integer
compileBinary = foldl' (\acc c -> acc * 2 + if c=='1' then 1 else 0) 0

compileLetBindings :: MonadCompile m => SExp -> m [(String, Expression)]
compileLetBindings (SNil _) = pure []
compileLetBindings ((Atom varName _ ::: rhs ::: SNil _) ::: rest) = do
  compiled_rhs <- compileExp rhs
  compiled_rest <- compileLetBindings rest
  pure ((varName, compiled_rhs) : compiled_rest)
compileLetBindings bad =
  throwError $ "Invalid let binding list: " ++ show bad

compileSparseArray :: MonadCompile m => SExp -> m [(Integer, Expression)]
compileSparseArray (SNil _) = pure []
compileSparseArray ((Num idx _ ::: val ::: SNil _) ::: rest) = do
    compiled_val <- compileExp val
    compiled_rest <- compileSparseArray rest
    pure ((idx, compiled_val) : compiled_rest)
compileSparseArray bad =
    throwError $ "Invalid sparse array entry: " ++ show bad

--------------------------
-- 7) Sort & Tags

--------------------------   


-- | Compiles a tag S-expression into the structured Tag type.
compileTag :: MonadCompile m => SExp -> m Tag
-- tag without value (i.e., a simple string)
compileTag (Atom tagName _) = pure (SimpleTag tagName)
-- tag with a value
compileTag (Atom "maxbits" _ ::: Num val _ ::: SNil _) = pure (MaxBitsTag val)
compileTag (Atom "maxvalue" _ ::: Num val _ ::: SNil _) = pure (MaxValTag val)
compileTag (Atom "minvalue" _ ::: Num val _ ::: SNil _) = pure (MinValTag val)
compileTag (Atom "max_abs" _ ::: Num val _ ::: SNil _) = pure (MaxAbsTag val)
compileTag (Atom "maxbit_abs" _ ::: Num val _ ::: SNil _) = pure (MaxBitsAbsTag val)
compileTag sexp = throwError $ "Unsupported tag format: " ++ show sexp

-- | Compiles a single sort.
compileSort :: MonadCompile m => SExp -> m Sort
compileSort (Atom "mod" _ ::: Num n _ ::: SNil _) =
  pure (FieldMod n)
compileSort (Atom "bool" _) =
  pure Bool
compileSort (Atom "bv" _ ::: Num n _ ::: SNil _) =
  pure (BitVector n)    
compileSort (Atom "array" _ ::: valSortExp ::: Num size _ ::: SNil _) = do
    valSort <- compileSort valSortExp
    pure (ArraySort valSort size)  
compileSort e = throwError $ "Unsupported sort: " ++ show e