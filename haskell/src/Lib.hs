module Lib
    ( parseYamelot
    ) where

import Text.ParserCombinators.Parsec

flowSequence = between (char '[') (char ']') $ sepEndBy flowNode (char ',')

flowNode = flowScalar

flowScalar = many1 $ char '1'

parseYamelot :: String -> Either ParseError [String]
parseYamelot input = parse flowSequence "(unknown)" input
