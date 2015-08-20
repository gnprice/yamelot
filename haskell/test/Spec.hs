module Main where

import System.Exit

import Lib

check :: (Show a, Show b, Eq b) => String -> (a -> b) -> (a, b) -> IO ()
check msg f (input, expected) = case f input of
      result | expected == result -> return ()
      result                      ->
         do putStrLn $ "failed " ++ msg ++ ": " ++ show input
            putStrLn $ "  expected: " ++ show expected
            putStrLn $ "  got: " ++ show result
            exitFailure

checkParse :: (String, Maybe Value) -> IO ()
checkParse = check "parse" (massage . parseYamelot)
  where massage (Right result) = Just result
        massage (Left _)       = Nothing

main :: IO ()
main = do mapM_ checkParse [
              ("[1,2a]", Just (Seq [Scalar "1", Scalar "2a"]))
            , ("[1]", Just (Seq [Scalar "1"]))
            , ("[]", Just (Seq []))
            , ("[1,[]]", Just (Seq [Scalar "1", Seq []]))
            , ("[,]", Nothing)
            , ("[1,]", Just (Seq [Scalar "1"]))
           ]
          mapM_ (check "annotate" annotateIndents) [
              ("ab\n cd", [(0,'a'),(1,'b'),(2,'\n'),(0,' '),(1,'c'),(2,'d')])
           ]
