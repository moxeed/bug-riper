module Sample where

a :: Bool
a = True

f :: Int -> Int
f a = if a > 10 then b else c
    where 
        b = if a < 20 then 1 else -1
        c = g a 0
f a = case a of 10 -> 1 
                _ -> 0

g a b = if a < 0 then 1 else -1
m (a:ax) = a

main :: IO ()
main = print $ f 10

-- FunBind
-- HsIf