{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.Arrays.ArraySelectOutOfBoundsTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isLeft, fromLeft)
import Data.List (isInfixOf)

spec :: Spec
spec = describe "Array Select Out-of-Bounds Check" $ do
  it "detects when an ArraySelect uses a known out-of-bounds index" $ do

    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- array 'arr' of size 3
    let arr = Binding { name = "arr", vid = 0, sort = ArraySort (FieldMod p) 3, tag = Nothing }
    -- scalar 'y'
    let y   = Binding { name = "y",   vid = 1, sort = FieldMod p, tag = Nothing }

    -- constraints 
    
    -- y === arr[5] (index 5 is out of bounds for array of size 3)
    let c1 = EqC 100 (Var "y") (ArraySelect (Var "arr") (Int 5))

    -- the test program
    let testProgram = Program
          { inputs          = [arr, y]
          , computationVars = []
          , constraintVars  = []
          , computations    = []
          , constraints     = [c1]
          , pfRecipExpressions = []
          , returnVars = []
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assertions

    -- checking that detectBugs returned an error (Left)
    bugResult `shouldSatisfy` isLeft

    let errors = fromLeft [] bugResult
    let errorString = unlines errors

    -- checking for the specific error message
    errorString `shouldSatisfy` ("Potential ArraySelect out-of-bounds: Index(es) [5]" `isInfixOf`)
    errorString `shouldSatisfy` ("used with array of size 3" `isInfixOf`)
