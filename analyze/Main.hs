module Main where
import HieASTGraphGenerator as AG
import HieDUGraphGenerator  as DU
import HieDUJungleGenerator as JU
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parse >>= putStr

parse :: [String] -> IO(String)
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse ["--ast", file] = AG.printAsts file
parse ["--du" , file] = DU.analyze file
parse ["--ju" , file, runFile] = JU.analyze file runFile
parse [] = usage   >> exit

usage :: IO()
usage   = putStrLn "Usage: analyze --ast file|--du file"
version :: IO()
version = putStrLn "Haskell analyze 0.1"
exit :: IO(String)
exit    = exitWith ExitSuccess
die :: IO(String)
die     = exitWith (ExitFailure 1)