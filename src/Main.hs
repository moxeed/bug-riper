module Main where
import Sample
import Sample1
import ME
import System.Environment

main :: IO()
main = print $ show tests

tests = [
    qsort ([0, 1]) 
    ]
