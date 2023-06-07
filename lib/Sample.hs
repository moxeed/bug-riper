module Sample
where 

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2) + g n

g :: Int -> Int
g a = if a > 3 then fib (a - 2) else fib (a - 1)