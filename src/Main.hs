module Main where
import Sample
import System.Environment

main :: IO()
main = print $ show tests

tests = [
    g 4 0 
   ,g 5 1 
    ]
