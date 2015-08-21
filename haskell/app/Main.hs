module Main where

import Indent

main :: IO ()
main = putStrLn $ show $ parseYamelot "[1,11,111]"
