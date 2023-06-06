module Sample where

g = 1

z :: Int -> Int
z a = (z' a) +  (z'' a) + (z''' a)

z' :: Int -> Int
z' a = if a > 2 then a else a

z'' :: Int -> Int
z'' 1 = 2
z'' 3 = 0
z'' _ = 0

z''' :: Int -> Int
z''' a = case a of 
    1 -> 2
    2 -> 3
    _ -> 0

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)
--Cannot Do This Therefore All of Function Declarations Are In Same Fun Bind Block
--z'' 4 = 5
--z'  1 = 5
--z'' 2 = 5