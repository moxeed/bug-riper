--{-# OPTIONS_GHC -fplugin SourcePlugin #-}
module Main where
--import Coverage
--import SortingNetwork
--import Control.Monad.State

-- cases = [
--     "applyGate1-input0", "applyGate1-g0", "applyGate1-gx0", "applyGate0-input0", "apply2-ix0", 
--     "apply2-gx0", "apply2-key0", "apply2-value0", "apply2-i2", "apply2-ix2", "apply2-value1", "apply2-i1", 
--     "apply2-i0", "apply1-i0", "apply0-value1", "apply1-gx1", "apply1-value1", "apply1-ix0", "apply1-gx0", 
--     "apply1-g0", "apply1-i1","apply0-value0"
--     ]

func a b = if b > 0 then c + 1 else c - 1
    where c = if a > b then 1 else -1   

main :: IO ()
main = print $ func 1 2
--main = print $ coverageScore coverageInfo
--main = print $ [ x | x <- cases, x `notElem` coverageInfo] 
--main = print $ coverageScore cases
--    where 
--        (_, coverageInfo) = runState coverage emptyCoverage
        --coverage = sort [1, 3, 2] [["-", "1", "1"]]
--        coverage = sort [1, 3, 2, 4] [["2", "1", "1", "2"], ["-", "1", "1", "-"]]
        --coverage = sort [1, 3, 2] []