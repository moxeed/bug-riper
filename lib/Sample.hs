module Sample
where 

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2) + g n n

g :: Int -> Int -> Int
g a b = if b > 0 then fib (a - 2) else fib (a - 1)