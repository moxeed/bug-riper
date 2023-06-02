module Main where

import HieDUGraphGenerator

main :: IO()
main =  do
    output <- analyze "./.hie/Sample.hie"
    writeFile "out.txt" output
