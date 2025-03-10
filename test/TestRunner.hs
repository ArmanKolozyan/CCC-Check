module Main where

import Test.Hspec
import qualified Syntax.ExpressionsParseTest
import qualified Analysis.FullyConstrainedTest
import qualified Syntax.IteParseTest

main :: IO ()
main = hspec $ do
    Syntax.ExpressionsParseTest.spec
    Analysis.FullyConstrainedTest.spec
    Syntax.IteParseTest.spec
