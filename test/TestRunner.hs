module Main where

import FullyConstrainedTest (runFullyConstrainedTest)
import IteParseTest (runIteParsingTest)
import System.Exit (exitFailure)
import ExpressionsParseTest (runExpressionParsingTest)

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