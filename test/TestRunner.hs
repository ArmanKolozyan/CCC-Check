module Main where

import FullyConstrainedTest (runFullyConstrainedTest)
import IteParseTest (runIteParsingTest)
import System.Exit (exitFailure)

main :: IO ()
main = do
  res1 <- runFullyConstrainedTest
  putStrLn "--------"
  res2 <- runIteParsingTest
  putStrLn "--------"
  if not (res1 && res2)
    then exitFailure
    else putStrLn "All tests succeeded!"