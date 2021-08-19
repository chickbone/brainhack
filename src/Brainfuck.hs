{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLabels #-}

module Brainfuck where

import Data.Attoparsec.ByteString (Parser, choice, many1)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.Maybe (catMaybes)
import Freer.Impl ()

data BF
  = Inc
  | Dec
  | Next
  | Prev
  | GetC
  | PutC
  | Loop [BF]

lexerBF :: Parser (Maybe BF)
lexerBF =
  choice
    [ Just Inc <$ char '+',
      Just Dec <$ char '-',
      Just Next <$ char '>',
      Just Prev <$ char '<',
      Just GetC <$ char ',',
      Just PutC <$ char '.',
      Just . Loop <$> loopBracket lexerBF
    ]

loopBracket :: Parser (Maybe a) -> Parser [a]
loopBracket f = catMaybes <$> (char '[' *> many1 f <* char ']')
