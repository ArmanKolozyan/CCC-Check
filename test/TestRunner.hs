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
import qualified Value_Inferencer.BugDetection.Edwards2MontgomeryWrongTest
import qualified Value_Inferencer.BugDetection.Edwards2MontgomeryFixedTest
import qualified Value_Inferencer.BugDetection.Montgomery2EdwardsWrongTest
import qualified Value_Inferencer.BugDetection.Montgomery2EdwardsFixedTest
import qualified Value_Inferencer.BugDetection.MontgomeryAddWrongTest
import qualified Value_Inferencer.BugDetection.MontgomeryAddFixedTest
import qualified Value_Inferencer.BugDetection.MontgomeryDoubleWrongTest
import qualified Value_Inferencer.BugDetection.MontgomeryDoubleFixedTest


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
    Value_Inferencer.BugDetection.Edwards2MontgomeryWrongTest.spec
    Value_Inferencer.BugDetection.Edwards2MontgomeryFixedTest.spec
    Value_Inferencer.BugDetection.Montgomery2EdwardsWrongTest.spec
    Value_Inferencer.BugDetection.Montgomery2EdwardsFixedTest.spec
    Value_Inferencer.BugDetection.MontgomeryAddWrongTest.spec
    Value_Inferencer.BugDetection.MontgomeryAddFixedTest.spec
    Value_Inferencer.BugDetection.MontgomeryDoubleWrongTest.spec
    Value_Inferencer.BugDetection.MontgomeryDoubleFixedTest.spec
