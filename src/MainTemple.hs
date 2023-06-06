module Main where
import Sample
import System.Environment

main :: IO()
main = do
    [arg] <- getArgs
    print $ run arg

run :: String -> String
