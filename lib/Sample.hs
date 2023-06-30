module Sample
where

fib :: Int -> Int
fib n 
    | n <= 2 = 1
    | otherwise = fib (n-1) + fib (n-2)

g :: Int -> Int -> Int
g a b = if b > 0 then fib (a - 2) else fib (a - 1)

m :: Int -> Int -> Int
m a b = case a of 1 -> b 
                  otherwise -> (-b)

limitOrder1 i bi shi p q s m fak = case m of (Just mq) -> id
                                             otherwise -> id

a = limitOrder1 1 2 3 4 5 6 Nothing 8