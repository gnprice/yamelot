module Main where

import System.Exit

import Lib

check :: (String, Maybe Value) -> IO ()
check (input, expected) = case parseYamelot input of
      Right result | expected == Just result -> return ()
      Left _       | expected == Nothing     -> return ()
      outcome                                ->
         do putStrLn $ "failed: " ++ input
            putStrLn $ "  expected: " ++ show expected
            putStrLn $ "  got: " ++ show outcome
            exitFailure

main :: IO ()
main = mapM_ check [ ("[1,2a]", Just (Seq [Scalar "1", Scalar "2a"]))
                   , ("[1]", Just (Seq [Scalar "1"]))
                   , ("[]", Just (Seq []))
                   , ("[,]", Nothing)
                   , ("[1,]", Just (Seq [Scalar "1"]))
                   ]
