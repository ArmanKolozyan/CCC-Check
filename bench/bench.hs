{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Control.DeepSeq

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax.AST
import ValueAnalysis.Analysis

setupProgram :: IO Program
setupProgram = do
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617
    let a = Binding {name = "a", vid = 0, sort = FieldMod p, tag = Just $ MaxValTag 3}
    let b = Binding {name = "b", vid = 1, sort = FieldMod p, tag = Just $ MaxValTag 3}
    let a_b0 = Binding {name = "a_b0", vid = 2, sort = Bool, tag = Just $ SimpleTag "binary"}
    let a_b1 = Binding {name = "a_b1", vid = 3, sort = Bool, tag = Just $ SimpleTag "binary"}
    let b_b0 = Binding {name = "b_b0", vid = 4, sort = Bool, tag = Just $ SimpleTag "binary"}
    let b_b1 = Binding {name = "b_b1", vid = 5, sort = Bool, tag = Just $ SimpleTag "binary"}
    let diff = Binding {name = "diff", vid = 6, sort = FieldMod p, tag = Just $ MaxValTag 6}
    let diff_b0 = Binding {name = "diff_b0", vid = 7, sort = Bool, tag = Just $ SimpleTag "binary"}
    let diff_b1 = Binding {name = "diff_b1", vid = 8, sort = Bool, tag = Just $ SimpleTag "binary"}
    let diff_b2 = Binding {name = "diff_b2", vid = 9, sort = Bool, tag = Just $ SimpleTag "binary"}
    let out = Binding {name = "out", vid = 10, sort = Bool, tag = Just $ SimpleTag "binary"}
    let cA_b0_binary = EqC 100 (Mul (Var "a_b0") (Sub (Var "a_b0") (Int 1))) (Int 0)
    let cA_b1_binary = EqC 101 (Mul (Var "a_b1") (Sub (Var "a_b1") (Int 1))) (Int 0)
    let cReconstructA = EqC 102 (Add (Var "a_b0") (Mul (Int 2) (Var "a_b1"))) (Var "a")
    let cB_b0_binary = EqC 103 (Mul (Var "b_b0") (Sub (Var "b_b0") (Int 1))) (Int 0)
    let cB_b1_binary = EqC 104 (Mul (Var "b_b1") (Sub (Var "b_b1") (Int 1))) (Int 0)
    let cReconstructB = EqC 105 (Add (Var "b_b0") (Mul (Int 2) (Var "b_b1"))) (Var "b")
    let cComputediff = EqC 106 (Var "diff") (Sub (Add (Var "a") (Int 4)) (Var "b"))
    let cdiff_b0_binary = EqC 107 (Mul (Var "diff_b0") (Sub (Var "diff_b0") (Int 1))) (Int 0)
    let cdiff_b1_binary = EqC 108 (Mul (Var "diff_b1") (Sub (Var "diff_b1") (Int 1))) (Int 0)
    let cdiff_b2_binary = EqC 109 (Mul (Var "diff_b2") (Sub (Var "diff_b2") (Int 1))) (Int 0)
    let cReconstructdiff = EqC 110 (Add (Add (Var "diff_b0") (Mul (Int 2) (Var "diff_b1"))) (Mul (Int 4) (Var "diff_b2"))) (Var "diff")
    let cOutDef = EqC 111 (Var "out") (Sub (Int 1) (Var "diff_b2"))
    let allConstraints = [ cA_b0_binary, cA_b1_binary, cReconstructA, cB_b0_binary, cB_b1_binary, cReconstructB, cComputediff, cdiff_b0_binary, cdiff_b1_binary, cdiff_b2_binary, cReconstructdiff, cOutDef ]
    let flattenedLessThan2Program =
          Program
            { inputs = [a, b],
              computationVars = [],
              constraintVars = [ a_b0, a_b1, b_b0, b_b1, diff, diff_b0, diff_b1, diff_b2, out ],
              computations = [],
              constraints = allConstraints,
              returnVars = [],
              pfRecipExpressions = []
            }
    return flattenedLessThan2Program

main :: IO ()
main = defaultMain [
  env setupProgram $ \program ->
    bgroup "ValueAnalysis" [
      bench "LessThan2" $ nf analyzeProgram program
    ]
  ]