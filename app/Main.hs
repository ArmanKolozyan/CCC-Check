module Main where    

import ValueAnalysis.Analysis

testFile :: String
testFile = "test/circir-testFiles/num2bits.circir" 

main :: IO ()
main = analyzeFromFile testFile