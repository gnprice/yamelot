module Lib
    ( parseYamelot
    , Value(..)
    , annotateIndents  -- urgh, exported for testing
    ) where

import Text.ParserCombinators.Parsec

data Value = Scalar String | Seq [Value]
  deriving (Eq, Show)


annotateIndents :: String -> [(Int, Char)]
annotateIndents = annotate 0
  where annotate n ('\n':s) = (n,'\n') : annotate 0 s
        annotate n (c:s)    = (n,c)    : annotate (n+1) s
        annotate _ []       = []


flowSequence = between (char '[') (char ']') $
                 Seq <$> sepEndBy flowNode (char ',')

flowNode = flowScalar <|> flowSequence

flowScalar = Scalar <$> (many1 $ alphaNum)

parseYamelot :: String -> Either ParseError Value
parseYamelot input = parse flowSequence "(unknown)" input
