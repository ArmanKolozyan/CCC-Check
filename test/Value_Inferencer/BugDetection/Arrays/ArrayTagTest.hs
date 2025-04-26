{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.Arrays.ArrayTagTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isLeft, fromLeft)
import Data.List (isInfixOf)

spec :: Spec
spec = describe "Array Tag Check" $ do
  it "detects when an array element violates the array's tag" $ do

    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- array 'arr' of size 3, tagged 'binary'
    let arr = Binding { name = "arr", vid = 0, sort = ArraySort (FieldMod p) 3, tag = Just (SimpleTag "binary") }
    -- scalar 'x'
    let x   = Binding { name = "x",   vid = 1, sort = FieldMod p, tag = Nothing }

    -- constraints
    
    -- arr[1] === x
    let c1 = EqC 100 (ArraySelect (Var "arr") (Int 1)) (Var "x")
    -- x === 5 (violates binary tag for arr[1])
    let c2 = EqC 101 (Var "x") (Int 5)

    let testProgram = Program
          { inputs          = [arr, x]
          , computationVars = []
          , constraintVars  = []
          , computations    = []
          , constraints     = [c1, c2]
          , pfRecipExpressions = []
          , returnVars = []
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assetions

    -- checking that detectBugs returned an error (Left)
    bugResult `shouldSatisfy` isLeft

    let errors = fromLeft [] bugResult
    let errorString = unlines errors

    -- checking for specific error messages about the tag violation on the array elements
    errorString `shouldSatisfy` ("Boolean variable `arr[1]` has values outside {0,1}: [5]" `isInfixOf`)
    errorString `shouldSatisfy` ("Boolean variable `arr[default]` has upper bound > 1" `isInfixOf`)
