module Main where

import Test.Hspec
import qualified Syntax.ExpressionsParseTest
import qualified CP_Analysis.Analysis.FullyConstrainedTest
import qualified Syntax.IteParseTest
import qualified Value_Inferencer.Analysis.Num2BitsTest
import qualified Value_Inferencer.Analysis.NonZeroTest
import qualified Value_Inferencer.Analysis.NonZeroTemplateTest
import qualified Value_Inferencer.Analysis.LessThanTest
import qualified Bug_Detector.SortTest
import qualified Value_Inferencer.Analysis.Edwards2MontgomeryWrongTest
import qualified Value_Inferencer.Analysis.Edwards2MontgomeryFixedTest
import qualified Value_Inferencer.Analysis.Montgomery2EdwardsWrongTest
import qualified Value_Inferencer.Analysis.Montgomery2EdwardsFixedTest
import qualified Value_Inferencer.Analysis.MontgomeryAddWrongTest
import qualified Value_Inferencer.Analysis.MontgomeryAddWrongTest
import qualified Value_Inferencer.Analysis.MontgomeryAddFixedTest


main :: IO ()
main = hspec $ do
    Syntax.ExpressionsParseTest.spec
    CP_Analysis.Analysis.FullyConstrainedTest.spec
    Syntax.IteParseTest.spec
    Value_Inferencer.Analysis.Num2BitsTest.spec
    Value_Inferencer.Analysis.NonZeroTest.spec
    Value_Inferencer.Analysis.NonZeroTemplateTest.spec
    Value_Inferencer.Analysis.LessThanTest.spec
    Bug_Detector.SortTest.spec
    Value_Inferencer.Analysis.Edwards2MontgomeryWrongTest.spec
    Value_Inferencer.Analysis.Edwards2MontgomeryFixedTest.spec
    Value_Inferencer.Analysis.Montgomery2EdwardsWrongTest.spec
    Value_Inferencer.Analysis.Montgomery2EdwardsFixedTest.spec
    Value_Inferencer.Analysis.MontgomeryAddWrongTest.spec
    Value_Inferencer.Analysis.MontgomeryAddFixedTest.spec
