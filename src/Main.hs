module Main where
import Sample
import Sample1
import System.Environment

main :: IO()
main = print $ show tests

tests = [
    smallerEq 0 ([1, 2, 3, 4, 5]) 
   ,smallerEq 0 ([0, 1, 2, 3]) 
   ,smallerEq 0 ([0, 1, 2, 4]) 
    ]
