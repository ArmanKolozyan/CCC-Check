module Main where    

import ValueAnalysis.Analysis

testFile :: String
testFile = "testFiles/num2bits.circir" 

main :: IO ()
main = analyzeFromFile testFile