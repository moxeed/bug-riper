module Sample1 
where
    
smallerEq :: Int -> [Int] -> [Int]
smallerEq = \v -> \list ->
    case list of
        [] -> []
        x:xs | x<=v -> x:smallerEq 1 xs
        _:xs -> smallerEq (v + 1) xs

-- Return the list which contains elements greater than the specified element
greater :: Int -> [Int] -> [Int]
greater = \v -> \list ->
    case list of
        [] -> []
        x:xs | x>v -> x:greater v xs
        _:xs -> greater v xs

qsort :: [Int] -> [Int]
qsort = \list ->
    case list of
        [] -> []
        x:xs -> 
            qsort (smallerEq x xs) ++ x:qsort (greater x xs)