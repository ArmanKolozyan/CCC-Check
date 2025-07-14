{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.BinSubTest (spec, binSubTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- | BinSub template test program
binSubTestProgram :: Program
binSubTestProgram = 
  let
    
    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- inputs (flattened 2x2 array of binary signals)
    -- representing in[0] = [in00, in01], in[1] = [in10, in11]
    in00 = Binding { name = "in00", vid = 0, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    in01 = Binding { name = "in01", vid = 1, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    in10 = Binding { name = "in10", vid = 2, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    in11 = Binding { name = "in11", vid = 3, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- outputs (flattened array of 2 binary signals)
    -- representing out = [out0, out1]
    out = Binding { name = "out", vid = 4, sort = ArraySort (FieldMod p) 2, tag = Just (SimpleTag "binary") }

    -- intermediate signals from Bits2Num(2) instances
    b2n1_out = Binding { name = "b2n1_out", vid = 5, sort = FieldMod p, tag = Just (MaxBitsTag 2) } 
    b2n2_out = Binding { name = "b2n2_out", vid = 6, sort = FieldMod p, tag = Just (MaxBitsTag 2) }

    -- intermediate signals from Num2Bits(3) instance (n=2, so n+1=3)
    n2b_in = Binding { name = "n2b_in", vid = 7, sort = FieldMod p, tag = Nothing }
    n2b_out0 = Binding { name = "n2b_out0", vid = 8, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    n2b_out1 = Binding { name = "n2b_out1", vid = 9, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    -- carry/borrow bit
    n2b_out2 = Binding { name = "n2b_out2", vid = 10, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- variable for the ignored carry/borrow bit
    aux_carry = Binding { name = "aux_carry", vid = 11, sort = FieldMod p, tag = Nothing }

    -- constraints

    -- Bits2Num(2) instance 1 (b2n1)
    -- b2n1_out === in00 * 1 + in01 * 2
    c_b2n1 = EqC 200 (Var "b2n1_out") (Add (Var "in00") (Mul (Int 2) (Var "in01")))

    -- Bits2Num(2) instance 2 (b2n2)
    -- b2n2_out === in10 * 1 + in11 * 2
    c_b2n2 = EqC 201 (Var "b2n2_out") (Add (Var "in10") (Mul (Int 2) (Var "in11")))

    -- Connection to Num2Bits(3) input
    -- n2b_in === 2^2 + b2n1_out - b2n2_out
    -- please see decodeSumOfPowers in Analysis.hs for the inference rule
    c_n2b_in = EqC 202 (Var "n2b_in") (Sub (Add (Int 4) (Var "b2n1_out")) (Var "b2n2_out"))

    -- Num2Bits(3) instance (n2b)
    -- binary checks (explicitly defined in Num2Bits template)
    c_n2b_bin0 = EqC 203 (Mul (Var "n2b_out0") (Sub (Var "n2b_out0") (Int 1))) (Int 0)
    c_n2b_bin1 = EqC 204 (Mul (Var "n2b_out1") (Sub (Var "n2b_out1") (Int 1))) (Int 0)
    c_n2b_bin2 = EqC 205 (Mul (Var "n2b_out2") (Sub (Var "n2b_out2") (Int 1))) (Int 0)
    -- reconstruction check
    -- n2b_in === n2b_out0 * 1 + n2b_out1 * 2 + n2b_out2 * 4
    c_n2b_recon = EqC 206 (Var "n2b_in") (Add (Var "n2b_out0")
                                                (Add (Mul (Int 2) (Var "n2b_out1"))
                                                     (Mul (Int 4) (Var "n2b_out2"))))

    -- connecting Num2Bits outputs to final 'out' array elements
    -- we define the 'out' array using ArrayConstruct
    c_out_def = EqC 207 (Var "out") (ArrayConstruct [Var "n2b_out0", Var "n2b_out1"] (ArraySort (FieldMod p) 2))

    -- connecting the ignored carry/borrow bit
    c_aux_carry = EqC 208 (Var "aux_carry") (Var "n2b_out2")

    -- all constraints
    allConstraints = [ c_b2n1, c_b2n2
                         , c_n2b_in
                         , c_n2b_bin0, c_n2b_bin1, c_n2b_bin2, c_n2b_recon
                         , c_out_def
                         , c_aux_carry
                         ]

  in Program
      { inputs          = [in00, in01, in10, in11] -- inputs whose tags are encorced!
      , computationVars = []
      , constraintVars  = [ out, b2n1_out, b2n2_out, n2b_in, n2b_out0, n2b_out1, n2b_out2, aux_carry ]
      , computations    = []
      , constraints     = allConstraints
      , pfRecipExpressions = []
      , returnVars = [out] -- expected output
      }

spec :: Spec
spec = describe "BinSub Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do
    -- running the bug detection
    let bugResult = detectBugs binSubTestProgram Nothing

    -- assertions 
    
    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight