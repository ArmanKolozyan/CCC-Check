module Main where

import Test.Hspec
import qualified Syntax.ExpressionsParseTest
import qualified CP_Analysis.Analysis.FullyConstrainedTest
import qualified Syntax.IteParseTest
import qualified Value_Inferencer.Analysis.Num2BitsTest
import qualified Value_Inferencer.Analysis.NonZeroTest

main :: IO ()
main = hspec $ do
    Syntax.ExpressionsParseTest.spec
    CP_Analysis.Analysis.FullyConstrainedTest.spec
    Syntax.IteParseTest.spec
    Value_Inferencer.Analysis.Num2BitsTest.spec
    Value_Inferencer.Analysis.NonZeroTest.spec
