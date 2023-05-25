module SortingNetwork
where
import Coverage


apply :: [Int] -> [String] -> String -> Int -> Coverage (Int, [Int])
apply [] [] _ value = (value, []) `covers` ["apply0-value0"]
apply (i:ix) (g:gx) "-" value 
    | g /= "-" = do
        (nextValue, rest) <- apply ix   gx g i >>= (`covers` ["apply1-ix0", "apply1-gx0", "apply1-g0", "apply1-i0"]) 
        (_, result)       <- apply rest gx "-" value >>= (`covers` ["apply1-gx1", "apply1-value1"])
        (value, min i nextValue:result) `covers` ["apply1-i1", "apply0-value1"]

apply (i:ix) (g:gx) key value
    | g == "-" = do
        (nextValue, rest) <- nextApply
        (nextValue, i:rest) `covers` ["apply2-i0"]
    | g /= key = do
        (nextValue, rest) <- nextApply
        (nextValue, i:rest) `covers` ["apply2-i1"]
    | g == key = do
        (nextValue, rest) <- nextApply
        (i, max value i:ix) `covers` ["apply2-i2", "apply2-ix2", "apply2-value1"]
    where nextApply = apply ix gx key value >>= (`covers` ["apply2-ix0", "apply2-gx0", "apply2-key0", "apply2-value0"]) 

applyGates :: [[String]] -> [Int] -> Coverage [Int]
applyGates []     input = input `covers` ["applyGate0-input0"]
applyGates (g:gx) input = do 
    (_, result) <- applyResult
    applyGates gx result >>= (`covers` ["applyGate1-gx0"])
    where applyResult = apply input g "-" 0 >>= (`covers` ["applyGate1-input0", "applyGate1-g0"])

sort :: [Int] -> [[String]] -> Coverage [Int]
sort input gates = applyGates gates input