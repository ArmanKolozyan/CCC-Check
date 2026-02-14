{-# LANGUAGE PatternSynonyms #-}

module Syntax.TagsParser (parseTags, parseTagsFile) where

import Syntax.AST (Tag(..))
import Syntax.Scheme.Parser (SExp(..), parseSexp, pattern (:::))
import qualified Data.Map.Strict as Map

-- | Parses a tags file content into a map from variable name to Tag.
parseTags :: String -> Either String (Map.Map String Tag)
parseTags input = do
    sexps <- parseSexp input
    case sexps of
        (firstSexp : _) -> parseTagsExp firstSexp
        []              -> Left "Error: empty tags file"

-- | Parses the top-level (tags ...) expression.
parseTagsExp :: SExp -> Either String (Map.Map String Tag)
parseTagsExp (Atom "tags" _ ::: entries) = do
    pairs <- parseEntries entries
    pure (Map.fromList pairs)
parseTagsExp _ = Left "Expected (tags ...) at top level"

-- | Parses a list of tag entries.
parseEntries :: SExp -> Either String [(String, Tag)]
parseEntries (SNil _) = pure []
parseEntries (entry ::: rest) = do
    pair <- parseEntry entry
    pairs <- parseEntries rest
    pure (pair : pairs)
parseEntries bad = Left $ "Invalid tag entries: " ++ show bad

-- | Parses a single tag entry like (varname binary) or (varname (maxbits 5)).
parseEntry :: SExp -> Either String (String, Tag)
-- Simple tag: (varname tagname)
parseEntry (Atom varName _ ::: Atom tagName _ ::: SNil _) =
    pure (varName, SimpleTag tagName)
-- Valued tag: (varname (maxbits N))
parseEntry (Atom varName _ ::: (Atom "maxbits" _ ::: Num val _ ::: SNil _) ::: SNil _) =
    pure (varName, MaxBitsTag val)
-- Valued tag: (varname (maxvalue N))
parseEntry (Atom varName _ ::: (Atom "maxvalue" _ ::: Num val _ ::: SNil _) ::: SNil _) =
    pure (varName, MaxValTag val)
-- Valued tag: (varname (minvalue N))
parseEntry (Atom varName _ ::: (Atom "minvalue" _ ::: Num val _ ::: SNil _) ::: SNil _) =
    pure (varName, MinValTag val)
-- Valued tag: (varname (max_abs N))
parseEntry (Atom varName _ ::: (Atom "max_abs" _ ::: Num val _ ::: SNil _) ::: SNil _) =
    pure (varName, MaxAbsTag val)
-- Valued tag: (varname (maxbit_abs N))
parseEntry (Atom varName _ ::: (Atom "maxbit_abs" _ ::: Num val _ ::: SNil _) ::: SNil _) =
    pure (varName, MaxBitsAbsTag val)
parseEntry bad = Left $ "Invalid tag entry: " ++ show bad

-- | Reads and parses a tags file.
parseTagsFile :: FilePath -> IO (Either String (Map.Map String Tag))
parseTagsFile path = parseTags <$> readFile path
