{-# LANGUAGE OverloadedStrings #-}

module ValueInference.BugDetection.TaggedCircomlib.SumTestDeep (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)
import ValueAnalysis.Analysis (analyzeProgram, VariableState(..))
import ValueAnalysis.ValueDomain (ValueDomain(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (fromList)

-- helper function to calculate nbits (log base 2 rounded up)
nbits :: Integer -> Integer
nbits 0 = 0
nbits x = floor (logBase 2 (fromIntegral x :: Double)) + 1

-- helper function to get domain by variable name
getDomainByName :: String -> Map.Map String VariableState -> ValueDomain
getDomainByName name states =
  case Map.lookup name states of
      Nothing -> error $ "Variable state not found for name " ++ show name
      Just st -> domain st

spec :: Spec
spec = describe "BinSum(2, 2) Template Test with Domain Checks" $ do
  it "successfully completes bug detection and infers correct domains" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- template parameters
    let n = 2
    let ops = 2
    let nout = n + nbits (ops - 1) -- nout = 2 + nbits(1) = 2 + 1 = 3

    -- bindings

    -- inputs (flattened ops*n array of binary signals)
    -- representing in[0] = [in00, in01], in[1] = [in10, in11]
    let in00 = Binding { name = "in00", vid = 0, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    let in01 = Binding { name = "in01", vid = 1, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    let in10 = Binding { name = "in10", vid = 2, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    let in11 = Binding { name = "in11", vid = 3, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- outputs (flattened array of nout binary signals)
    -- representing out = [out0, out1, out2]
    let out = Binding { name = "out", vid = 4, sort = ArraySort (FieldMod p) nout, tag = Just (SimpleTag "binary") }

    -- intermediate signals from Bits2Num(n) instances (aux0, aux1)
    let aux0 = Binding { name = "aux0", vid = 5, sort = FieldMod p, tag = Just (MaxBitsTag n) }
    let aux1 = Binding { name = "aux1", vid = 6, sort = FieldMod p, tag = Just (MaxBitsTag n) }

    -- intermediate signals from Num2Bits(nout) instance (n2b)
    let n2b_in = Binding { name = "n2b_in", vid = 8, sort = FieldMod p, tag = Nothing }
    let n2b_out0 = Binding { name = "n2b_out0", vid = 9, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    let n2b_out1 = Binding { name = "n2b_out1", vid = 10, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    let n2b_out2 = Binding { name = "n2b_out2", vid = 11, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- constraints

    -- Bits2Num(n) instance for in[0] -> aux0
    -- aux0 === in00 * 2^0 + in01 * 2^1
    let c_aux0 = EqC 300 (Var "aux0") (Add (Var "in00") (Mul (Int 2) (Var "in01")))

    -- Bits2Num(n) instance for in[1] -> aux1
    -- aux1 === in10 * 2^0 + in11 * 2^1
    let c_aux1 = EqC 301 (Var "aux1") (Add (Var "in10") (Mul (Int 2) (Var "in11")))

    -- summation: result = aux0 + aux1 IS NOT A CONSTRAINT, IT IS A COMPILE-TIME 
    -- INTERMEDIATE VAR WHICH WILL BE ELIMINATED BY CirC

    -- connection to Num2Bits(nout) input
    -- n2b_in <== result
    let c_n2b_in = EqC 303 (Var "n2b_in") (Add (Var "aux0") (Var "aux1"))

    -- Num2Bits(nout) instance (n2b) constraints
    -- binary checks (explicitly defined in Num2Bits template)
    let c_n2b_bin0 = EqC 304 (Mul (Var "n2b_out0") (Sub (Var "n2b_out0") (Int 1))) (Int 0)
    let c_n2b_bin1 = EqC 305 (Mul (Var "n2b_out1") (Sub (Var "n2b_out1") (Int 1))) (Int 0)
    let c_n2b_bin2 = EqC 306 (Mul (Var "n2b_out2") (Sub (Var "n2b_out2") (Int 1))) (Int 0)
    -- reconstruction check
    -- n2b_in === n2b_out0 * 2^0 + n2b_out1 * 2^1 + n2b_out2 * 2^2
    -- please see decodeSumOfPowers in Analysis.hs for the inference rule
    let c_n2b_recon = EqC 307 (Var "n2b_in") (Add (Var "n2b_out0")
                                                (Add (Mul (Int 2) (Var "n2b_out1"))
                                                     (Mul (Int 4) (Var "n2b_out2"))))

    -- connecting Num2Bits outputs to final 'out' array elements
    -- out <== n2b.out
    let c_out_def = EqC 308 (Var "out") (ArrayConstruct [Var "n2b_out0", Var "n2b_out1", Var "n2b_out2"] (ArraySort (FieldMod p) nout))

    -- all constraints
    let allConstraints = [ c_aux0, c_aux1
                         , c_n2b_in
                         , c_n2b_bin0, c_n2b_bin1, c_n2b_bin2, c_n2b_recon
                         , c_out_def
                         ]

    -- the test program
    let testProgram = Program
          { inputs          = [in00, in01, in10, in11] -- inputs whose tags are enforced
          , computationVars = []
          , constraintVars  = [ out, aux0, aux1, n2b_in, n2b_out0, n2b_out1, n2b_out2 ]
          , computations    = []
          , constraints     = allConstraints
          , pfRecipExpressions = []
          , returnVars = [out] -- expected output
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assertions

    -- checking that no bugs were detected
    bugResult `shouldSatisfy` isRight

    -- assertions for inferred domains

    -- running the full analysis to get final states
    let finalStates = analyzeProgram testProgram

    -- inputs should be binary
    getDomainByName "in00" finalStates `shouldBe` BoundedValues {lowerBound = Just 0, upperBound = Just 1, gaps = fromList []}
    getDomainByName "in01" finalStates `shouldBe` BoundedValues {lowerBound = Just 0, upperBound = Just 1, gaps = fromList []}
    getDomainByName "in10" finalStates `shouldBe` BoundedValues {lowerBound = Just 0, upperBound = Just 1, gaps = fromList []}
    getDomainByName "in11" finalStates `shouldBe` BoundedValues {lowerBound = Just 0, upperBound = Just 1, gaps = fromList []}

    -- Bits2Num outputs (constrained by inputs and MaxValTag)
    getDomainByName "aux0" finalStates `shouldBe` BoundedValues {lowerBound = Just 0, upperBound = Just 3, gaps = fromList []}
    getDomainByName "aux1" finalStates `shouldBe` BoundedValues {lowerBound = Just 0, upperBound = Just 3, gaps = fromList []}

    -- Num2Bits input (sum of aux0, aux1 => range [0..6])
    getDomainByName "n2b_in" finalStates `shouldBe` KnownValues (Set.fromList [0, 1, 2, 3, 4, 5, 6])

    -- Num2Bits outputs (constrained by binary checks and reconstruction)
    getDomainByName "n2b_out0" finalStates `shouldBe` KnownValues (Set.fromList [0, 1])
    getDomainByName "n2b_out1" finalStates `shouldBe` KnownValues (Set.fromList [0, 1])
    getDomainByName "n2b_out2" finalStates `shouldBe` KnownValues (Set.fromList [0, 1])

    -- final output array 'out'
    -- expecting an ArrayDomain where elements 0, 1, 2 are binary
    case getDomainByName "out" finalStates of
      ArrayDomain elemMap defDom size -> do
        size `shouldBe` nout
        Map.lookup 0 elemMap `shouldBe` Just (KnownValues (Set.fromList [0, 1]))
        Map.lookup 1 elemMap `shouldBe` Just (KnownValues (Set.fromList [0, 1]))
        Map.lookup 2 elemMap `shouldBe` Just (KnownValues (Set.fromList [0, 1]))
      otherDomain -> expectationFailure $ "Expected ArrayDomain for 'out', but got: " ++ show otherDomain