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
checkParse False = check "parse" (checkFail . flip run yamelot) True
  where checkFail Nothing = True
        checkFail (Just r) = length (snd r) > 0

main :: IO ()
main = do mapM_ (uncurry checkParse) [
              (True,  "- [bb]")
            , (True,  "- a")
            , (True,  " - a")
            , (False, "- [bb]a")
            , (True,  "-\n -\n  -ax")
            , (False, " -\n -\n  -ax")
            , (True,  "- [ b   ] ")
            , (True,  "-\n - \n  [ bb,\n ac] ")
            , (True,  "-\n  -\n    [bbbb, cc ,\n  aa,]")
            , (True,  "-\n  -\n    [bbbb, cc ,\n  aa,]")
            , (True,  "-\n -\n  [1,\n2]")
            -- Literal scalars
            , (False, "-\n -\n       |\n ab")
            , (True,  "-\n -\n       |\n  ab")
            , (True,  "-\n -\n       |\n  ab\n")
            , (True,  "-\n -\n       |\n   ab")
            , (True,  "- |\n  a\n- b")
            , (True,  " - |\n   a\n - b")
            , (True,  "- |\n  a\n  b")
            , (False, "- |\n     a\n  b")
            , (True,  "- |\n  a\n     b")
         -- , (True,  "- |\n a\n\n b")
            , (True,  "- |\n a\n \n b")
            , (True,  "- |\n a\n  \n b") -- and preserves that space
            ]
