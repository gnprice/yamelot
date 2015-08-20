module Lib
    ( parseYamelot
    ) where

import Text.ParserCombinators.Parsec

flowSequence = do r <- char '[' >> (inner <|> return [])
                  char ']'
                  return r
  where inner = do first <- flowNode
                   rest <- continued
                   return $ first:rest
        continued = (char ',' >> (inner <|> return [])) <|> return []

flowNode = flowScalar

flowScalar = many1 $ char '1'

parseYamelot :: String -> Either ParseError [String]
parseYamelot input = parse flowSequence "(unknown)" input
