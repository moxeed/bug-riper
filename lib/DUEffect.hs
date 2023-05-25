module DUEffect where

func :: Int -> Int -> Int
func a b = if b > 0 then c + 1 else c - 1
    where c = if a > 0 then 1 else -1