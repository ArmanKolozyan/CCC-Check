{-# LANGUAGE OverloadedStrings #-}
module ValueAnalysis.UserRules (UserRule(..), UserAction(..),
                  parseUserRules, matchConstraint) where

import Syntax.AST
import Syntax.Compiler (parseAndCompileConstraint)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- | A user rule is:
--  * 'patternC', the constraint pattern (the user wrote something like (= ...))
--  * 'actions', a list of domain restrictions like "x in {0,1}", "x in [0..3]", etc.
data UserRule = UserRule
  { patternC :: Constraint
  , actions  :: [UserAction]
  } deriving (Eq, Show)

data UserAction
  = ConstrainSet String (Set.Set Integer)      -- e.g. "x in {0,1}"
  | ConstrainRange String Integer Integer      -- e.g. "x in [0..3]"
  deriving (Eq, Show)

----------------------------------------------------
-- 1) Parsing user rule files
-- Suppose the user writes something like:
-- RULE
-- PATTERN: (= (* x (- x 1)) 0)
-- ACTION: x in {0,1}
----------------------------------------------------

parseUserRules :: String -> Either String [UserRule]
parseUserRules content = do
    let ls = lines content
    parseRulesFromLines ls []

parseRulesFromLines :: [String] -> [UserRule] -> Either String [UserRule]
parseRulesFromLines [] acc = Right (reverse acc) -- reversing is not really necessary
parseRulesFromLines ("RULE":rest) acc =
  case break (=="RULE") rest of
    (block, restLines) ->
      -- block is lines until next "RULE" or EOF
      case parseSingleBlock block of
        Left err -> Left err
        Right rule -> parseRulesFromLines restLines (rule:acc)
parseRulesFromLines _ _ = Left "Expected 'RULE' or EOF"

-- A single block looks like:
-- PATTERN: <someConstraintInASTSyntax>
-- ACTION: x in {0,1}
-- ACTION: f in [0..3]
parseSingleBlock :: [String] -> Either String UserRule
parseSingleBlock ls =
  let (patLines, actionLines) = span (not . isActionLine) ls
  in case patLines of
       (pline:ps) | isPatternLine pline ->
         let patternStr = drop (length ("PATTERN: " :: String) ) pline
         in case parseAndCompileConstraint patternStr of
              Left e -> Left e
              Right c ->
                -- parsing actions
                let acts = map parseAction (filter isActionLine actionLines)
                in case sequence acts of
                     Left err -> Left err
                     Right goodActs -> Right (UserRule c goodActs)
       _ -> Left "No pattern line found in block"

isPatternLine :: String -> Bool
isPatternLine s = ("PATTERN:" `elem` words s)

isActionLine :: String -> Bool
isActionLine s = ("ACTION:" `elem` words s)

parseAction :: String -> Either String UserAction
parseAction s =
  -- e.g. "ACTION: x in {0,1}" or "ACTION: f in [0..3]"
  let trimmed = drop (length ("ACTION: " :: String)) s
  in case words trimmed of
       -- e.g. "x", "in", "{0,1}"
       [varName, "in", setStr]
         | head setStr == '{'
           -> let inside = drop 1 (take (length setStr - 1) setStr) -- droping braces
                  nums  = splitOn ',' inside
                  parsedNums = map read nums
              in Right (ConstrainSet varName (Set.fromList parsedNums))

         | head setStr == '['
           -> let inside = drop 1 (take (length setStr - 1) setStr) -- e.g. "0..3"
                  [lo,hi] = splitOn '.' inside -- e.g. [0,3]
              in Right (ConstrainRange varName (read lo) (read hi))

       _ -> Left $ "Invalid action: " ++ s

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim str =
  let (chunk, rest) = break (== delim) str
  in case rest of
       [] -> [chunk]
       (_:after) -> chunk : splitOn delim after

-----------------------------------------------------
-- 2) Pattern matching user rule constraints
-----------------------------------------------------

-- We do a "unification" approach:
-- We unify the user pattern constraint with the actual constraint from the program.
-- If the user pattern is EqC c1 c2, then the actual must be EqC c1' c2'
-- We unify c1 with c1' (which yields a map placeholder->realVar), unify c2 with c2', etc.
-- If unification succeeds, we get (Map String String) containing "placeholder x -> realVar varName"
matchConstraint :: Constraint -> Constraint
                -> Maybe (Map.Map String String)
matchConstraint patC realC =
  unifyConstraint patC realC Map.empty

unifyConstraint :: Constraint -> Constraint
                -> Map.Map String String
                -> Maybe (Map.Map String String)
unifyConstraint (EqC _ pe1 pe2) (EqC _ re1 re2) acc = do
  acc'  <- unifyExpr pe1 re1 acc
  acc'' <- unifyExpr pe2 re2 acc'
  pure acc''
unifyConstraint _ _ _ = Nothing

unifyExpr :: Expression -> Expression
          -> Map.Map String String
          -> Maybe (Map.Map String String)
unifyExpr (Var placeholder) (Var realVar) acc =
  -- if placeholder is something we haven't bound yet, we add the mapping:
  case Map.lookup placeholder acc of
    Nothing -> Just (Map.insert placeholder realVar acc)
    Just oldReal | oldReal == realVar -> Just acc
                 | otherwise -> Nothing -- contradiction
unifyExpr (Int i1) (Int i2) acc =
  if i1==i2 then Just acc else Nothing
unifyExpr (Add a1 a2) (Add b1 b2) acc = do
  acc'  <- unifyExpr a1 b1 acc
  acc'' <- unifyExpr a2 b2 acc'
  pure acc''
unifyExpr (Mul a1 a2) (Mul b1 b2) acc = do
  acc'  <- unifyExpr a1 b1 acc
  acc'' <- unifyExpr a2 b2 acc'
  pure acc''
unifyExpr _ _ _ = Nothing

-----------------------------------------------------
-- 3) Applying user actions
-----------------------------------------------------

-- moved to Analysis file

-----------------------------------------------------
-- All together
-----------------------------------------------------

-- moved to Analysis file