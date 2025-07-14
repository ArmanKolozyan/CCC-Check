{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.SumTest (spec, sumTestProgram) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)

-- helper function to calculate nbits (log base 2 rounded up)
nbits :: Integer -> Integer
nbits 0 = 0
nbits x = floor (logBase 2 (fromIntegral x :: Double)) + 1

-- | BinSum(2, 2) template test program
sumTestProgram :: Program
sumTestProgram =
  let
    
    -- using BN254 as prime field for demonstration
    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- template parameters
    n = 2
    ops = 2
    nout = n + nbits (ops - 1) -- nout = 2 + nbits(1) = 2 + 1 = 3

    -- bindings

    -- inputs (flattened ops*n array of binary signals)
    -- representing in[0] = [in00, in01], in[1] = [in10, in11]
    in00 = Binding { name = "in00", vid = 0, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    in01 = Binding { name = "in01", vid = 1, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    in10 = Binding { name = "in10", vid = 2, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    in11 = Binding { name = "in11", vid = 3, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- outputs (flattened array of nout binary signals)
    -- representing out = [out0, out1, out2]
    out = Binding { name = "out", vid = 4, sort = ArraySort (FieldMod p) nout, tag = Just (SimpleTag "binary") }

    -- intermediate signals from Bits2Num(n) instances (aux0, aux1)
    aux0 = Binding { name = "aux0", vid = 5, sort = FieldMod p, tag = Just (MaxBitsTag n) }
    aux1 = Binding { name = "aux1", vid = 6, sort = FieldMod p, tag = Just (MaxBitsTag n) }

    -- intermediate signals from Num2Bits(nout) instance (n2b)
    n2b_in = Binding { name = "n2b_in", vid = 8, sort = FieldMod p, tag = Nothing }
    n2b_out0 = Binding { name = "n2b_out0", vid = 9, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    n2b_out1 = Binding { name = "n2b_out1", vid = 10, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    n2b_out2 = Binding { name = "n2b_out2", vid = 11, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- constraints

    -- Bits2Num(n) instance for in[0] -> aux0
    -- aux0 === in00 * 2^0 + in01 * 2^1
    c_aux0 = EqC 300 (Var "aux0") (Add (Var "in00") (Mul (Int 2) (Var "in01")))

    -- Bits2Num(n) instance for in[1] -> aux1
    -- aux1 === in10 * 2^0 + in11 * 2^1
    c_aux1 = EqC 301 (Var "aux1") (Add (Var "in10") (Mul (Int 2) (Var "in11")))

    -- summation: result = aux0 + aux1 IS NOT A CONSTRAINT, IT IS A COMPILE-TIME 
    -- INTERMEDIATE VAR WHICH WILL BE ELIMINATED BY CirC

    -- connection to Num2Bits(nout) input
    -- n2b_in <== result
    c_n2b_in = EqC 303 (Var "n2b_in") (Add (Var "aux0") (Var "aux1"))

    -- Num2Bits(nout) instance (n2b) constraints
    -- binary checks (explicitly defined in Num2Bits template)
    c_n2b_bin0 = EqC 304 (Mul (Var "n2b_out0") (Sub (Var "n2b_out0") (Int 1))) (Int 0)
    c_n2b_bin1 = EqC 305 (Mul (Var "n2b_out1") (Sub (Var "n2b_out1") (Int 1))) (Int 0)
    c_n2b_bin2 = EqC 306 (Mul (Var "n2b_out2") (Sub (Var "n2b_out2") (Int 1))) (Int 0)

    -- reconstruction check
    -- n2b_in === n2b_out0 * 2^0 + n2b_out1 * 2^1 + n2b_out2 * 2^2
    -- please see decodeSumOfPowers in Analysis.hs for the inference rule
    c_n2b_recon = EqC 307 (Var "n2b_in") (Add (Var "n2b_out0")
                                          (Add (Mul (Int 2) (Var "n2b_out1"))
                                               (Mul (Int 4) (Var "n2b_out2"))))

    -- connecting Num2Bits outputs to final 'out' array elements
    -- out <== n2b.out
    c_out_def = EqC 308 (Var "out") (ArrayConstruct [Var "n2b_out0", Var "n2b_out1", Var "n2b_out2"] (ArraySort (FieldMod p) nout))

    -- all constraints
    allConstraints = [ c_aux0, c_aux1
                     , c_n2b_in
                     , c_n2b_bin0, c_n2b_bin1, c_n2b_bin2, c_n2b_recon
                     , c_out_def
                     ]

  in Program
       { inputs          = [in00, in01, in10, in11] -- inputs whose tags are enforced
       , computationVars = []
       , constraintVars  = [ out, aux0, aux1, n2b_in, n2b_out0, n2b_out1, n2b_out2 ]
       , computations    = []
       , constraints     = allConstraints
       , pfRecipExpressions = []
       , returnVars = [out] -- expected output
       }

spec :: Spec
spec = describe "BinSum(2, 2) Template Test" $ do
  it "successfully completes analysis without detecting bugs" $ do

    -- running the bug detection
    let bugResult = detectBugs sumTestProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight
