module Main where

import Lib

main :: IO ()
main = putStrLn $ show $ parseYamelot "[1,11,111]"
