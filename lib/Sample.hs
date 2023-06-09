module Sample
where

fib :: Int -> Int
fib n 
    | n <= 2 = 1
    | otherwise = fib (n-1) + fib (n-2)

g :: Int -> Int -> Int
g a b = if b > 0 then fib (a - 2) else fib (a - 1)