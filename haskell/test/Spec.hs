module Main where

import System.Exit

import Lib

check :: (String, [String]) -> IO ()
check (input, expected) = case parseYamelot input of
      Right result | result == expected -> return ()
      _                                 -> exitFailure

main :: IO ()
main = mapM_ check [ ("[1,11]", ["1","11"]) ]
