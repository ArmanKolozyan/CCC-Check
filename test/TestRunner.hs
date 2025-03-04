module Main where

import Syntax.FullyConstrainedTest (runFullyConstrainedTest)
import Syntax.IteParseTest (runIteParsingTest)
import Syntax.ExpressionsParseTest (runExpressionParsingTest)
import System.Exit (exitFailure)

main :: IO ()
main = do
  res1 <- runFullyConstrainedTest
  putStrLn "--------"
  res2 <- runIteParsingTest
  putStrLn "--------"
  res3 <- runExpressionParsingTest
  putStrLn "--------"
  if not (res1 && res2)
    then exitFailure
    else putStrLn "All tests succeeded!"