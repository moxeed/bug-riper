module DUEffect where
    
import Debug.Trace

func2 :: Int -> Int -> Int
func2 a b = if b > 0 then c + 1 else c - 1
    where c = if a > 0 then 1 else -1 

func :: Int -> Int -> Int
func a b = func2 a b
