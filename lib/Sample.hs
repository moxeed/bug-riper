module Sample where

z :: Int -> Int
z a = (z' a) +  (z'' a) + (z''' a)

z' :: Int -> Int
z' a = if a > 2 then 1 else -1

z'' :: Int -> Int
z'' 1 = 2
z'' 3 = 0
z'' _ = 0

z''' :: Int -> Int
z''' a = case a of 
    1 -> 2
    2 -> 3
    _ -> 0