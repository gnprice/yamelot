module Main where

import System.Exit

import Indent

check :: (Show a, Show b, Eq b) => String -> (a -> b) -> b -> a -> IO ()
check msg f expected input = case f input of
      result | expected == result -> return ()
      result                      ->
         do putStrLn $ "failed " ++ msg ++ ": " ++ show input
            putStrLn $ "  expected: " ++ show expected
            putStrLn $ "  got: " ++ show result
            exitFailure

checkParse :: Bool -> String -> IO ()
checkParse True = check "parse" (fmap snd . flip run yamelot) (Just "")
checkParse False = check "parse" (fmap ((>0) . length . snd) . flip run yamelot) (Just True)


main :: IO ()
main = do mapM_ (uncurry checkParse) [
              (True, "- [b]")
            , (True, "- a")
            , (False, "- [b]a")
            , (True, "-\n -\n  -a")
            , (True, "- [ b   ] ")
            , (True, "-\n - \n  [ b\n b] ")
            ]
