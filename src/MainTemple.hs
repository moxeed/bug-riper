module Main where
import Sample
import Sample1
import ME
import System.Environment

main :: IO()
main = do
    [arg] <- getArgs
    print $ run arg

run :: String -> String
