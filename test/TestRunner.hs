module Main where

import Test.Hspec
import qualified Syntax.ExpressionsParseTest
import qualified CP_Analysis.Analysis.FullyConstrainedTest
import qualified Syntax.IteParseTest

main :: IO ()
main = hspec $ do
    Syntax.ExpressionsParseTest.spec
    CP_Analysis.Analysis.FullyConstrainedTest.spec
    Syntax.IteParseTest.spec
