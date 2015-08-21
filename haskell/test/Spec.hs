module Main where

import System.Exit

import Indent

check :: (Show a, Show b, Eq b) => String -> (a -> b) -> (a, b) -> IO ()
check msg f (input, expected) = case f input of
      result | expected == result -> return ()
      result                      ->
         do putStrLn $ "failed " ++ msg ++ ": " ++ show input
            putStrLn $ "  expected: " ++ show expected
            putStrLn $ "  got: " ++ show result
            exitFailure

checkParse :: String -> IO ()
checkParse input = check "parse" (fmap snd . flip run yamelot) (input, Just "")

main :: IO ()
main = do mapM_ checkParse [
              ("- [b]")
            ]
