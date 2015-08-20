module Main where

import Lib

main :: IO ()
main = mapM_ putStrLn $ parseYamelot "[1,11,111]"
