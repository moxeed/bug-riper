module SortingNetwork
where

apply :: [Int] -> [String] -> String -> Int -> (Int, [Int])
apply _ _ _ value = (value, [])
apply (i:ix) (g:gx) "-" value 
    | g /= "-" = (value, min i nextValue:result)
    where
        (nextValue, rest) = apply ix   gx g i
        (_, result)       = apply rest gx "-" value

apply (i:ix) (g:gx) key value
    | g == "-" = (nextValue, i:rest)
    | g /= key = (nextValue, i:rest)
    | otherwise = (i, max value i:ix)
    where (nextValue, rest) = apply ix gx key value

applyGates :: [[String]] -> [Int] -> [Int]
applyGates []     input = input
applyGates (g:gx) input = applyGates gx result
    where (_, result) = apply input g "-" 0

sort :: [Int] -> [[String]] -> [Int]
sort input gates = applyGates gates input