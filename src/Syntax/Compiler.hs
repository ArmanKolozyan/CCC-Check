module Syntax.Compiler (compile, parseAndCompile) where

import Syntax.AST
import Control.Monad ((>=>))
import Control.Monad.Except
import Data.Char (isDigit)

import Syntax.Scheme.Parser
import qualified Syntax.Scheme.Parser as SExp

--------------------------
-- 1) Top-level
--------------------------

type MonadCompile m = (MonadError String m)

-- | Takes a top-level s-expression (SExp) and produces our Program AST.
compile :: MonadCompile m => SExp -> m Program
compile (Atom "computations" _ ::: (Atom _name _ ::: compBody) ::: SNil _) =
  throwError "correct one"
compile sexp =
  throwError $ "Expected (computations (NAME (computation ...))), got: " ++ show sexp

-- | parses and compiles the given IR representation to AST nodes.
parseAndCompile :: String -> Either String Program
parseAndCompile = fmap head . parseSexp >=> compile
{-
parseAndCompile source = do
    sExpr <- parseSexp source
    compileProgram sExpr
    -}