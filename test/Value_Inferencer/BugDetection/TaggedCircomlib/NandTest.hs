{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.NandTest (spec, nandTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | NAND Gate template test program
nandTestProgram :: Program
nandTestProgram = 
  let
    
    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- inputs
    a = Binding { name = "a", vid = 0, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    b = Binding { name = "b", vid = 1, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- output
    out = Binding { name = "out", vid = 2, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- constraints

    -- NAND gate constraint: out <== 1 - a*b
    c_nand_def = EqC 100 (Var "out") (Sub (Int 1) (Mul (Var "a") (Var "b")))

    -- all constraints
    allConstraints = [ c_nand_def ]

  in Program
      { inputs          = [a, b] -- inputs whose tags are enforced
      , computationVars = []
      , constraintVars  = [ out ]
      , computations    = []
      , constraints     = allConstraints
      , pfRecipExpressions = []
      , returnVars = [out] -- expected output
      }

spec :: Spec
spec = describe "NAND Gate Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do
    -- running the bug detection
    let bugResult = detectBugs nandTestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight