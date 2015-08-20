module Main where

import System.Exit

import Lib

check :: (String, Maybe [String]) -> IO ()
check (input, expected) = case parseYamelot input of
      Right result | expected == Just result -> return ()
      Left _       | expected == Nothing     -> return ()
      outcome                                ->
         do putStrLn $ "failed: " ++ input
            putStrLn $ "  expected: " ++ show expected
            putStrLn $ "  got: " ++ show outcome
            exitFailure

main :: IO ()
main = mapM_ check [ ("[1,11]", Just ["1","11"])
                   , ("[1]", Just ["1"])
                   , ("[]", Just [])
                   , ("[,]", Nothing)
                   , ("[1,]", Just ["1"])
                   ]
