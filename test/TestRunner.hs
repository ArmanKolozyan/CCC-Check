module Main where

import Test.Hspec
import qualified Syntax.ExpressionsParseTest
import qualified Syntax.IteParseTest
import qualified ValueInference.ValueAnalysis.Num2BitsTest
import qualified ValueInference.ValueAnalysis.NonZeroTest
import qualified ValueInference.ValueAnalysis.NonZeroTemplateTest
import qualified ValueInference.ValueAnalysis.LessThanTest
import qualified ValueInference.BugDetection.SortTest
import qualified ValueInference.BugDetection.Veridise.Edwards2MontgomeryWrongTest
import qualified ValueInference.BugDetection.Veridise.Edwards2MontgomeryFixedTest
import qualified ValueInference.BugDetection.Veridise.Montgomery2EdwardsWrongTest
import qualified ValueInference.BugDetection.Veridise.Montgomery2EdwardsFixedTest
import qualified ValueInference.BugDetection.Veridise.MontgomeryAddWrongTest
import qualified ValueInference.BugDetection.Veridise.MontgomeryAddFixedTest
import qualified ValueInference.BugDetection.Veridise.MontgomeryDoubleWrongTest
import qualified ValueInference.BugDetection.Veridise.MontgomeryDoubleFixedTest
import qualified ValueInference.BugDetection.TrailOfBits.ToBinaryTest
import qualified ValueInference.BugDetection.TrailOfBits.EnforceAuthTest
import qualified ValueInference.BugDetection.Arrays.ArrayTagTest
import qualified ValueInference.BugDetection.Arrays.ArraySelectOutOfBoundsTest
import qualified ValueInference.BugDetection.Arrays.ArrayStoreOutOfBoundsTest
import qualified ValueInference.BugDetection.TaggedCircomlib.DecoderTestRaw
import qualified ValueInference.BugDetection.TaggedCircomlib.DecoderTestBinary
import qualified ValueInference.BugDetection.TaggedCircomlib.DecoderTestNoOutputTag
import qualified ValueInference.BugDetection.TaggedCircomlib.BinSubTest
import qualified ValueInference.BugDetection.TaggedCircomlib.BinSubTestDeep
import qualified ValueInference.BugDetection.TaggedCircomlib.BinSumTest
import qualified ValueInference.BugDetection.TaggedCircomlib.SumTestDeep
import qualified ValueInference.BugDetection.TaggedCircomlib.NotTest
import qualified ValueInference.BugDetection.TaggedCircomlib.XorTest
import qualified ValueInference.BugDetection.TaggedCircomlib.AndTest
import qualified ValueInference.BugDetection.TaggedCircomlib.OrTest
import qualified ValueInference.BugDetection.TaggedCircomlib.NandTest
import qualified ValueInference.BugDetection.TaggedCircomlib.NorTest
import qualified ValueInference.BugDetection.TaggedCircomlib.IsZeroTest
import qualified ValueInference.BugDetection.TaggedCircomlib.IsZeroForcedTest
import qualified ValueInference.BugDetection.TaggedCircomlib.IsEqualTest
import qualified ValueInference.BugDetection.TaggedCircomlib.IsEqualForcedTest
import qualified ValueInference.BugDetection.TaggedCircomlib.Num2BitsTest
import qualified ValueInference.BugDetection.TaggedCircomlib.Bits2NumTest
import qualified ValueInference.BugDetection.TaggedCircomlib.LessThanTest
import qualified ValueInference.BugDetection.TaggedCircomlib.GreaterThanTest
import qualified ValueInference.BugDetection.TaggedCircomlib.GreaterEqThanTest
import qualified ValueInference.BugDetection.TaggedCircomlib.LessEqThanTest
import qualified ValueInference.BugDetection.TaggedCircomlib.BigLessThanTest
import qualified ValueInference.BugDetection.TaggedCircomlib.ModSumTest
import qualified ValueInference.BugDetection.TaggedCircomlib.MultiMux1Test
import qualified ValueInference.BugDetection.TaggedCircomlib.Mux1Test

main :: IO ()
main = hspec $ do
    Syntax.ExpressionsParseTest.spec
    Syntax.IteParseTest.spec
    ValueInference.ValueAnalysis.Num2BitsTest.spec
    ValueInference.ValueAnalysis.NonZeroTest.spec
    ValueInference.ValueAnalysis.NonZeroTemplateTest.spec
    ValueInference.ValueAnalysis.LessThanTest.spec
    ValueInference.BugDetection.SortTest.spec
    ValueInference.BugDetection.Veridise.Edwards2MontgomeryWrongTest.spec
    ValueInference.BugDetection.Veridise.Edwards2MontgomeryFixedTest.spec
    ValueInference.BugDetection.Veridise.Montgomery2EdwardsWrongTest.spec
    ValueInference.BugDetection.Veridise.Montgomery2EdwardsFixedTest.spec
    ValueInference.BugDetection.Veridise.MontgomeryAddWrongTest.spec
    ValueInference.BugDetection.Veridise.MontgomeryAddFixedTest.spec
    ValueInference.BugDetection.Veridise.MontgomeryDoubleWrongTest.spec
    ValueInference.BugDetection.Veridise.MontgomeryDoubleFixedTest.spec
    ValueInference.BugDetection.TrailOfBits.ToBinaryTest.spec
    ValueInference.BugDetection.TrailOfBits.EnforceAuthTest.spec
    ValueInference.BugDetection.Arrays.ArrayTagTest.spec
    ValueInference.BugDetection.Arrays.ArraySelectOutOfBoundsTest.spec
    ValueInference.BugDetection.Arrays.ArrayStoreOutOfBoundsTest.spec
    ValueInference.BugDetection.TaggedCircomlib.DecoderTestBinary.spec
    ValueInference.BugDetection.TaggedCircomlib.DecoderTestNoOutputTag.spec
    ValueInference.BugDetection.TaggedCircomlib.BinSubTestDeep.spec
    ValueInference.BugDetection.TaggedCircomlib.SumTestDeep.spec
    ValueInference.BugDetection.TaggedCircomlib.ModSumTest.spec
    ValueInference.BugDetection.TaggedCircomlib.NotTest.spec
    ValueInference.BugDetection.TaggedCircomlib.XorTest.spec
    ValueInference.BugDetection.TaggedCircomlib.AndTest.spec
    ValueInference.BugDetection.TaggedCircomlib.OrTest.spec
    ValueInference.BugDetection.TaggedCircomlib.NandTest.spec
    ValueInference.BugDetection.TaggedCircomlib.NorTest.spec
    ValueInference.BugDetection.TaggedCircomlib.IsZeroTest.spec
    ValueInference.BugDetection.TaggedCircomlib.IsZeroForcedTest.spec
    ValueInference.BugDetection.TaggedCircomlib.DecoderTestRaw.spec
    ValueInference.BugDetection.TaggedCircomlib.IsEqualTest.spec
    ValueInference.BugDetection.TaggedCircomlib.IsEqualForcedTest.spec
    ValueInference.BugDetection.TaggedCircomlib.Num2BitsTest.spec
    ValueInference.BugDetection.TaggedCircomlib.Bits2NumTest.spec
    ValueInference.BugDetection.TaggedCircomlib.MultiMux1Test.spec
    ValueInference.BugDetection.TaggedCircomlib.Mux1Test.spec
    ValueInference.BugDetection.TaggedCircomlib.LessThanTest.spec
    ValueInference.BugDetection.TaggedCircomlib.GreaterThanTest.spec
    ValueInference.BugDetection.TaggedCircomlib.GreaterEqThanTest.spec
    ValueInference.BugDetection.TaggedCircomlib.LessEqThanTest.spec
    ValueInference.BugDetection.TaggedCircomlib.BigLessThanTest.spec
    ValueInference.BugDetection.TaggedCircomlib.BinSubTest.spec
    ValueInference.BugDetection.TaggedCircomlib.BinSumTest.spec


