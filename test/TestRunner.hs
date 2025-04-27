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
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.SumTest
import qualified Value_Inferencer.BugDetection.TaggedCircomlib.SumTestDeep


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
    Value_Inferencer.BugDetection.TaggedCircomlib.DecoderTestRaw.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.DecoderTestBinary.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.DecoderTestNoOutputTag.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.BinSubTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.BinSubTestDeep.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.SumTest.spec
    Value_Inferencer.BugDetection.TaggedCircomlib.SumTestDeep.spec
