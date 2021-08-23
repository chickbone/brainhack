{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Brainfuck (BFProgram, loop, putC, getC, prev, next, dec, inc, runBF, formatBF, evalBFCode, genCProgram, rebuildBF) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, put)
import Control.Monad.Writer (tell)
import Data.Attoparsec.ByteString (Parser, choice, many1, parseOnly)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Function (fix)
import Freer.Impl (Freer, genFreer, interpretM, singleton)
import Freer.State (StateT, evalStateT)
import Freer.Writer (Writer, runWriter)
import Memory (Memory, center, emptyMemory, left, mapCenter, right)
import System.IO (hFlush, stdout)

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
lexerBF = many1 bf
  where
    bf =
      choice
        [ Inc <$ char '+',
          Dec <$ char '-',
          Next <$ char '>',
          Prev <$ char '<',
          GetC <$ char ',',
          PutC <$ char '.',
          Loop <$> loopB
        ]
    loopB = genFreer <$> (char '[' *> many1 bf <* char ']')

interpretBF :: BFProgram r -> StateT Memory IO r
interpretBF = interpretM $ \case
  Inc -> modify $! mapCenter (+ 1)
  Dec -> modify $! mapCenter (subtract 1)
  Next -> modify left
  Prev -> modify right
  GetC -> liftIO getChar >>= modify . mapCenter . const . fromIntegral . fromEnum
  PutC -> do
    mem <- get
    let c = center mem
    liftIO $ print c
    liftIO $ hFlush stdout
    put mem
  Loop pg -> fix $ \goto -> do
    mem <- get
    let c = center mem
    put mem
    if c == 0
      then pure ()
      else interpretBF pg >> goto

logBF :: BFProgram a -> Writer ByteString a
logBF = interpretM $ \case
  Inc -> tell "+"
  Dec -> tell "-"
  Next -> tell ">"
  Prev -> tell "<"
  GetC -> tell ","
  PutC -> tell "."
  Loop bf -> tell "[" >> logBF bf >> tell "]"

translateC :: BFProgram a -> ByteString
translateC = capturer . snd . runWriter . writerC
  where
    writerC :: BFProgram a -> Writer ByteString a
    writerC = interpretM $ \case
      Inc -> tell "(*p)++;"
      Dec -> tell "(*p)--;"
      Next -> tell "++p;"
      Prev -> tell "--p;"
      GetC -> tell "getchar(*p);"
      PutC -> tell "putchar(*p);"
      Loop bf -> tell "while(*p){" >> writerC bf >> tell "}"
    capturer = ("#include <stdio.h> int main(){int m[30000]={};int *p=m;" <>) . (<> "}")

rebuildBF :: BFProgram a -> ByteString
rebuildBF = snd . runWriter . logBF

runBF :: BFProgram a -> IO a
runBF = flip evalStateT (emptyMemory 30000) . interpretBF

formatBF :: ByteString -> ByteString
formatBF = B.filter (`elem` ("+-><,.[]" :: String))

evalBFCode :: ByteString -> IO ()
evalBFCode code = case parseOnly lexerBF (formatBF code) of
  Left err -> print err
  Right pg -> runBF $ genFreer pg

genCProgram :: ByteString -> ByteString
genCProgram code = case parseOnly lexerBF (formatBF code) of
  Left err -> B.pack err
  Right pg -> translateC $ genFreer pg

inc, dec, next, prev, getC, putC :: BFProgram ()
inc = singleton Inc
dec = singleton Dec
next = singleton Next
prev = singleton Prev
getC = singleton GetC
putC = singleton PutC

loop :: BFProgram a -> BFProgram ()
loop = singleton . Loop
