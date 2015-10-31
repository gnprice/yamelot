module Main where

import Data.List (intercalate)
import System.Environment
import System.IO
import System.Console.GetOpt
import System.Exit

import Indent

-- Converting whole input to JSON

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

yamelotToJson :: IO ()
yamelotToJson = do
      input <- getContents
      case run input yamelot of
          Just (v, s) -> putStrLn $ dumpJson v
          Nothing -> do
              putStrLn "error!"
              exitFailure

-- Running a sequence of tests

processTests :: IO ()
processTests = do
      hPutStrLn stderr "coming soon!"
      exitFailure

-- The command line

data Options = Options { optTest :: Bool }

startOptions :: Options
startOptions = Options { optTest = False }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option "t" ["test"]
      (NoArg (\opt -> return opt { optTest = True }))
      "Treat the input as a set of test cases"
  , Option "" ["help"]
      (NoArg (\_ -> do name <- getProgName
                       hPutStrLn stderr (usageInfo name options)
                       exitSuccess))
      "Show a usage message"
  ]

main :: IO ()
main = do
    args <- getArgs
    let (actions, positionals, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    if optTest opts then
      processTests
    else
      yamelotToJson
