module Main where
import Sample
import System.Environment

main :: IO()
main = print $ show tests

tests = [
    g 5 0 
   ,g 6 1 
   ,g 4 0 
    ]
