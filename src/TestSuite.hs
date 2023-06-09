module Main where
import Sample
import System.Environment

main :: IO()
main = print $ show tests

tests = [
    fib 4 
    ]
