module Main where

import Test.Hspec
import qualified Syntax.ExpressionsParseTest
import qualified CP_Analysis.Analysis.FullyConstrainedTest
import qualified Syntax.IteParseTest
import qualified Value_Inferencer.ValueAnalysis.Num2BitsTest
import qualified Value_Inferencer.ValueAnalysis.NonZeroTest
import qualified Value_Inferencer.ValueAnalysis.NonZeroTemplateTest
import qualified Value_Inferencer.ValueAnalysis.LessThanTest
import qualified Value_Inferencer.BugDetection.SortTest
import qualified Value_Inferencer.BugDetection.Veridise.Edwards2MontgomeryWrongTest
import qualified Value_Inferencer.BugDetection.Veridise.Edwards2MontgomeryFixedTest
import qualified Value_Inferencer.BugDetection.Veridise.Montgomery2EdwardsWrongTest
import qualified Value_Inferencer.BugDetection.Veridise.Montgomery2EdwardsFixedTest
import qualified Value_Inferencer.BugDetection.Veridise.MontgomeryAddWrongTest
import qualified Value_Inferencer.BugDetection.Veridise.MontgomeryAddFixedTest
import qualified Value_Inferencer.BugDetection.Veridise.MontgomeryDoubleWrongTest
import qualified Value_Inferencer.BugDetection.Veridise.MontgomeryDoubleFixedTest
import qualified Value_Inferencer.BugDetection.TrailOfBits.ToBinaryTest
import qualified Value_Inferencer.BugDetection.TrailOfBits.EnforceAuthTest
import qualified Value_Inferencer.BugDetection.Arrays.ArrayTagTest
import qualified Value_Inferencer.BugDetection.Arrays.ArraySelectOutOfBoundsTest
import qualified Value_Inferencer.BugDetection.Arrays.ArrayStoreOutOfBoundsTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.DecoderTestRaw
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.DecoderTestBinary
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.DecoderTestNoOutputTag
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.BinSubTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.BinSubTestDeep
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.BinSumTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.SumTestDeep
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.NotTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.XorTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.AndTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.OrTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.NandTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.NorTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.IsZeroTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.IsZeroForcedTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.IsEqualTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.IsEqualForcedTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.Num2BitsTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.Bits2NumTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.LessThanTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.GreaterThanTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.GreaterEqThanTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.LessEqThanTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.BigLessThanTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.ModSumTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.MultiMux1Test
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.Mux1Test

main :: IO ()
main = hspec $ do
    Syntax.ExpressionsParseTest.spec
    Syntax.IteParseTest.spec
    CP_Analysis.Analysis.FullyConstrainedTest.spec
    Value_Inferencer.ValueAnalysis.Num2BitsTest.spec
    Value_Inferencer.ValueAnalysis.NonZeroTest.spec
    Value_Inferencer.ValueAnalysis.NonZeroTemplateTest.spec
    Value_Inferencer.ValueAnalysis.LessThanTest.spec
    Value_Inferencer.BugDetection.SortTest.spec
    Value_Inferencer.BugDetection.Veridise.Edwards2MontgomeryWrongTest.spec
    Value_Inferencer.BugDetection.Veridise.Edwards2MontgomeryFixedTest.spec
    Value_Inferencer.BugDetection.Veridise.Montgomery2EdwardsWrongTest.spec
    Value_Inferencer.BugDetection.Veridise.Montgomery2EdwardsFixedTest.spec
    Value_Inferencer.BugDetection.Veridise.MontgomeryAddWrongTest.spec
    Value_Inferencer.BugDetection.Veridise.MontgomeryAddFixedTest.spec
    Value_Inferencer.BugDetection.Veridise.MontgomeryDoubleWrongTest.spec
    Value_Inferencer.BugDetection.Veridise.MontgomeryDoubleFixedTest.spec
    Value_Inferencer.BugDetection.TrailOfBits.ToBinaryTest.spec
    Value_Inferencer.BugDetection.TrailOfBits.EnforceAuthTest.spec
    Value_Inferencer.BugDetection.Arrays.ArrayTagTest.spec
    Value_Inferencer.BugDetection.Arrays.ArraySelectOutOfBoundsTest.spec
    Value_Inferencer.BugDetection.Arrays.ArrayStoreOutOfBoundsTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.DecoderTestBinary.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.DecoderTestNoOutputTag.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.BinSubTestDeep.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.SumTestDeep.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.ModSumTest.spec

    Value_Inferencer.BugDetection.TaggedCircomlib.NotTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.XorTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.AndTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.OrTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.NandTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.NorTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.IsZeroTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.IsZeroForcedTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.DecoderTestRaw.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.IsEqualTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.IsEqualForcedTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.Num2BitsTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.Bits2NumTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.MultiMux1Test.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.Mux1Test.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.LessThanTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.GreaterThanTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.GreaterEqThanTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.LessEqThanTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.BigLessThanTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.BinSubTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.BinSumTest.spec


