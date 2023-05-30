module Sample where

a :: Int
a = 10

f a = if b == False then a else 0
    where b = a > 10
f 1 = case a of 10 -> 1 
                _ -> 0
g a b = a
m (a:ax) = a

main :: IO ()
main = print $ f a

-- FunBind
-- HsIf