module Main where
import Sample
import System.Environment

main :: IO()
main = print $ show tests

tests = [
    enqueueOrder' (LimitOrder 2 3 4 0 1 Buy (Just 5) True) [LimitOrder 9 7 4 6 8 Buy Nothing False, LimitOrder 9 7 4 6 8 Buy Nothing False] 
    ]
