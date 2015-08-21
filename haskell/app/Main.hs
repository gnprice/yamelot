module Main where

import Data.List (intercalate)
import Indent
import System.Exit

escape :: Char -> String
escape c = case c of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    c' -> [c']

dumpJson :: Value -> String
dumpJson (Scalar s) = "\""++(concat $ map escape s)++"\""
dumpJson (Sequence vs) = "["++(intercalate "," $ map dumpJson vs)++"]"

main :: IO ()
main = do
    input <- getContents
    case run input yamelot of
        Just (v, s) -> putStrLn $ dumpJson v
        Nothing -> do
            putStrLn "error!"
            exitFailure
