{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module CPDomain.Domain where

import Data.Singletons
import Domain (inject)
import Lattice (Joinable)
import Lattice.ConstantPropagationLattice
import Data.TypeLevel.HMap
import Data.Map (Map)

-- | The labels for the sparse labeled product.
-- We currently only have integers, but since we will add other types later,
-- I decided to already use a sparse labeled product.
-- VRAAG: als je CP wilt doen voor alle mogelijke types, heb je dan een sparse labeled product nodig?
-- Of is het enkel als je CP wilt doen voor een bepaalde type, interval domain wilt gebruiken voor een ander
-- type, etc.?
data CirCKey = IntKey | BoolKey deriving (Eq, Ord, Show)    

-- This is necessary for the infrastructure promoting
-- data constructors to types and the other way around.
-- VRAAG: ?
$(genHKeys ''CirCKey)

-- | A type-level mapping which represents the definition
-- of the contents of the sparse labeled product.
-- In our current setup, 'M' contains a single mapping:
-- IntKey ::-> CP Integer
-- This means that each 'CirCVal' stores a value for the field 'IntKey'
-- which is a constant-propagation lattice over integers. This design is
-- extensible: we can later add more keys (and thus more abstract
-- properties) without changing the rest of the analysis infrastructure.
type M = '[IntKey ::-> CP Integer, BoolKey ::-> CP Bool]

-- | 'CirCVal' represents the abstract values of our program.
--
-- At the type level, 'CirCVal' is defined as a sparse labeled product using an HMap.
-- An HMap is a heterogeneous map: its keys are determined at compile–time (here by the type–level
-- list 'M'), and each key (or “field”) is associated with a value from a particular abstract domain.
--
-- At runtime, a 'CirCVal' is represented as an HMap containing only the fields that have been set.
-- For example, if variable "x" is known to be 5, its abstract value is represented as:
--     CirCVal (singleton @IntKey (Constant 5))
-- Here, 'singleton' creates an HMap with one field. The key 'IntKey' is known at compile–time,
-- but at runtime it appears as a tag that helps us determine its domain.
-- VRAAG: klopt dit een beetje? "a tag that helps us determine its domain"
newtype CirCVal = CirCVal (HMap M) deriving (Eq, Joinable)

-- To print CirC values.
instance Show CirCVal where
  show (CirCVal hmap) = 
    "CirCVal " ++ show (mapList select hmap)
    where
      select :: forall k. Sing (k :: CirCKey) -> Assoc k M -> (String, String)
      select SIntKey cpVal = ("IntKey", show cpVal)

-- | Injects a number in the domain.
num :: Integer -> CirCVal
num = CirCVal . singleton @IntKey . inject

-- | Environment type alias: we use a Map from variable names to our abstract value.
type Env = Map String CirCVal