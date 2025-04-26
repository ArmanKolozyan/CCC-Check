{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.Arrays.ArrayStoreOutOfBoundsTest (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isLeft, fromLeft)
import Data.List (isInfixOf)

spec :: Spec
spec = describe "Array Store Out-of-Bounds Check" $ do
  it "detects when an ArrayStore uses a known out-of-bounds index" $ do

    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- array 'arr' of size 3
    let arr = Binding { name = "arr", vid = 0, sort = ArraySort (FieldMod p) 3, tag = Nothing }
    -- scalar 'z' used as the value to store
    let z   = Binding { name = "z",   vid = 1, sort = FieldMod p, tag = Nothing }

    -- constraints

    -- Constraint involving storing at index 5 (out of bounds for size 3).
    -- We embed the ArrayStore within a constraint (here: equating it to itself)
    -- so collectArrayAccesses finds it.
    let storeExpr = ArrayStore (Var "arr") (Int 5) (Var "z")
    let c1 = EqC 100 storeExpr storeExpr

    -- the test program
    let testProgram = Program
          { inputs          = [arr, z]
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
    errorString `shouldSatisfy` ("Potential ArrayStore out-of-bounds: Index(es) [5]" `isInfixOf`)
    errorString `shouldSatisfy` ("used with array of size 3" `isInfixOf`)
