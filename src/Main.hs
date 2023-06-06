module Main where
import Sample
import System.Environment

main :: IO()
main = do
    [arg] <- getArgs
    print $ run arg

run :: String -> String
run "z 1 " = show $ z 1 
run "z 3 " = show $ z 3 
run "z 2 " = show $ z 2 
run "z 4 " = show $ z 4 
