{-# LANGUAGE OverloadedStrings #-}

module Value_Inferencer.BugDetection.TaggedCircomlib.BinSubTestDeep (spec) where

import Test.Hspec
import Syntax.AST
import BugDetection.BugDetection
import Data.Either (isRight)
import ValueAnalysis.Analysis (analyzeProgram, VariableState(..))
import ValueAnalysis.ValueDomain (ValueDomain(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (fromList)

-- helper function to get domain by variable name
getDomainByName :: String -> Map.Map String VariableState -> ValueDomain
getDomainByName name states =
  case Map.lookup name states of
      Nothing -> error $ "Variable state not found for name " ++ show name
      Just st -> domain st

spec :: Spec
spec = describe "BinSub Template Test with Domain Checks" $ do
  it "successfully completes bug detection and infers correct domains" $ do

    -- using BN254 as prime field for demonstration
    let p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    -- bindings

    -- inputs (flattened 2x2 array of binary signals)
    -- representing in[0] = [in00, in01], in[1] = [in10, in11]
    let in00 = Binding { name = "in00", vid = 0, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    let in01 = Binding { name = "in01", vid = 1, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    let in10 = Binding { name = "in10", vid = 2, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    let in11 = Binding { name = "in11", vid = 3, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- outputs (flattened array of 2 binary signals)
    -- representing out = [out0, out1]
    let out = Binding { name = "out", vid = 4, sort = ArraySort (FieldMod p) 2, tag = Just (SimpleTag "binary") }

    -- intermediate signals from Bits2Num(2) instances
    let b2n1_out = Binding { name = "b2n1_out", vid = 5, sort = FieldMod p, tag = Just (MaxBitsTag 2) } 
    let b2n2_out = Binding { name = "b2n2_out", vid = 6, sort = FieldMod p, tag = Just (MaxBitsTag 2) }

    -- intermediate signals from Num2Bits(3) instance (n=2, so n+1=3)
    let n2b_in = Binding { name = "n2b_in", vid = 7, sort = FieldMod p, tag = Nothing }
    let n2b_out0 = Binding { name = "n2b_out0", vid = 8, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    let n2b_out1 = Binding { name = "n2b_out1", vid = 9, sort = FieldMod p, tag = Just (SimpleTag "binary") }
    -- carry/borrow bit
    let n2b_out2 = Binding { name = "n2b_out2", vid = 10, sort = FieldMod p, tag = Just (SimpleTag "binary") }

    -- variable for the ignored carry/borrow bit
    let aux_carry = Binding { name = "aux_carry", vid = 11, sort = FieldMod p, tag = Nothing }

    -- constraints

    -- Bits2Num(2) instance 1 (b2n1)
    -- b2n1_out === in00 * 1 + in01 * 2
    let c_b2n1 = EqC 200 (Var "b2n1_out") (Add (Var "in00") (Mul (Int 2) (Var "in01")))

    -- Bits2Num(2) instance 2 (b2n2)
    -- b2n2_out === in10 * 1 + in11 * 2
    let c_b2n2 = EqC 201 (Var "b2n2_out") (Add (Var "in10") (Mul (Int 2) (Var "in11")))

    -- Connection to Num2Bits(3) input
    -- n2b_in === 2^2 + b2n1_out - b2n2_out
    -- please see decodeSumOfPowers in Analysis.hs for the inference rule
    let c_n2b_in = EqC 202 (Var "n2b_in") (Sub (Add (Int 4) (Var "b2n1_out")) (Var "b2n2_out"))

    -- Num2Bits(3) instance (n2b)
    -- binary checks (explicitly defined in Num2Bits template)
    let c_n2b_bin0 = EqC 203 (Mul (Var "n2b_out0") (Sub (Var "n2b_out0") (Int 1))) (Int 0)
    let c_n2b_bin1 = EqC 204 (Mul (Var "n2b_out1") (Sub (Var "n2b_out1") (Int 1))) (Int 0)
    let c_n2b_bin2 = EqC 205 (Mul (Var "n2b_out2") (Sub (Var "n2b_out2") (Int 1))) (Int 0)
    -- reconstruction check
    -- n2b_in === n2b_out0 * 1 + n2b_out1 * 2 + n2b_out2 * 4
    let c_n2b_recon = EqC 206 (Var "n2b_in") (Add (Var "n2b_out0")
                                                (Add (Mul (Int 2) (Var "n2b_out1"))
                                                     (Mul (Int 4) (Var "n2b_out2"))))

    -- connecting Num2Bits outputs to final 'out' array elements
    -- we define the 'out' array using ArrayConstruct
    let c_out_def = EqC 207 (Var "out") (ArrayConstruct [Var "n2b_out0", Var "n2b_out1"] (ArraySort (FieldMod p) 2))

    -- connecting the ignored carry/borrow bit
    let c_aux_carry = EqC 208 (Var "aux_carry") (Var "n2b_out2")

    -- all constraints
    let allConstraints = [ c_b2n1, c_b2n2
                         , c_n2b_in
                         , c_n2b_bin0, c_n2b_bin1, c_n2b_bin2, c_n2b_recon
                         , c_out_def
                         , c_aux_carry
                         ]

    -- the test program
    let testProgram = Program
          { inputs          = [in00, in01, in10, in11] -- inputs whose tags are encorced!
          , computationVars = []
          , constraintVars  = [ out, b2n1_out, b2n2_out, n2b_in, n2b_out0, n2b_out1, n2b_out2, aux_carry ]
          , computations    = []
          , constraints     = allConstraints
          , pfRecipExpressions = []
          , returnVars = [out] -- expected output
          }

    -- running the bug detection
    let bugResult = detectBugs testProgram Nothing

    -- assertions for bugs 
    
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

    -- Bits2Num outputs (constrained by inputs and MaxBitsTag)
    getDomainByName "b2n1_out" finalStates `shouldBe` BoundedValues {lowerBound = Just 0, upperBound = Just 3, gaps = fromList []}
    getDomainByName "b2n2_out" finalStates `shouldBe` BoundedValues {lowerBound = Just 0, upperBound = Just 3, gaps = fromList []}

    -- Num2Bits input (4 + [0..3] - [0..3] => range [1..7])
    getDomainByName "n2b_in" finalStates `shouldBe` KnownValues (fromList [1,2,3,4,5,6,7])

    -- Num2Bits outputs (constrained by binary checks and reconstruction)
    getDomainByName "n2b_out0" finalStates `shouldBe` KnownValues (Set.fromList [0, 1])
    getDomainByName "n2b_out1" finalStates `shouldBe` KnownValues (Set.fromList [0, 1])
    getDomainByName "n2b_out2" finalStates `shouldBe` KnownValues (Set.fromList [0, 1])

    -- final output array 'out'
    -- expecting an ArrayDomain where elements 0 and 1 are binary
    case getDomainByName "out" finalStates of
      ArrayDomain elemMap defDom size -> do
        size `shouldBe` 2
        Map.lookup 0 elemMap `shouldBe` Just (KnownValues (Set.fromList [0, 1]))
        Map.lookup 1 elemMap `shouldBe` Just (KnownValues (Set.fromList [0, 1]))
      otherDomain -> expectationFailure $ "Expected ArrayDomain for 'out', but got: " ++ show otherDomain

    -- ignored carry bit
    getDomainByName "aux_carry" finalStates `shouldBe` KnownValues (Set.fromList [0, 1])