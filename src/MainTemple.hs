module Main where
import Sample
import Sample1
import A
import B
import ME
import System.Environment

main :: IO()
main = do
    [arg] <- getArgs
    print $ run arg

run :: String -> String
