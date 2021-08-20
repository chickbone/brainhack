{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Brainfuck where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify, put)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Attoparsec.ByteString (Parser, choice, endOfInput, many1, manyTill)
import Data.Attoparsec.ByteString.Char8 (anyChar, char)
import Data.Function (fix)
import Data.Maybe (catMaybes)
import Freer.Impl
import Memory (Memory, center, emptyMemory, left, mapCenter, right)

type BFProgram a = Freer BF a

data BF a where
  Inc :: BF ()
  Dec :: BF ()
  Next :: BF ()
  Prev :: BF ()
  GetC :: BF ()
  PutC :: BF ()
  Loop :: BFProgram a -> BF ()

lexerBF :: Parser [BF ()]
lexerBF = catMaybes <$> manyTill bf endOfInput
  where
    bf =
      choice
        [ Just Inc <$ char '+',
          Just Dec <$ char '-',
          Just Next <$ char '>',
          Just Prev <$ char '<',
          Just GetC <$ char ',',
          Just PutC <$ char '.',
          Just . Loop <$> loop,
          Nothing <$ anyChar
        ]
    loopBracket f = catMaybes <$> (char '[' *> many1 f <* char ']')
    loop = do
      pg <- loopBracket bf
      return $ mapM_ singleton pg

interpretBF :: BFProgram r -> StateT Memory IO r
interpretBF = interpretM $ \case
  Inc -> modify $ mapCenter (+ 1)
  Dec -> modify $ mapCenter (subtract 1)
  Next -> modify left
  Prev -> modify right
  GetC -> liftIO getChar >>= modify . mapCenter . const . fromIntegral . fromEnum
  PutC -> do
    mem <- get
    let c = center mem
    liftIO $ putChar . toEnum . fromIntegral $ c
    put mem
  Loop pg -> fix $ \loop -> do
    mem <- get
    let c = center mem
    if c == 0
      then put mem
      else interpretBF pg >> loop

logBF :: BFProgram a -> Writer String a
logBF = interpretM $ \case
  Inc -> tell "+"
  Dec -> tell "-"
  Next -> tell ">"
  Prev -> tell "<"
  GetC -> tell ","
  PutC -> tell "."
  Loop bf -> tell "[" >> logBF bf >> tell "]"

rebuildBF :: BFProgram a -> String
rebuildBF = snd . runWriter . logBF

runBF :: BFProgram a -> IO a
runBF = flip evalStateT (emptyMemory 30000) . interpretBF

inc, dec, next, prev, getC, putC :: BFProgram ()
inc = singleton Inc
dec = singleton Dec
next = singleton Next
prev = singleton Prev
getC = singleton GetC
putC = singleton GetC
