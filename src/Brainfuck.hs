{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Brainfuck (BFProgram, loop, putC, getC, prev, next, dec, inc, runBF, formatCode, evalBFCode, genCProgram, composeBFCode, mov, add) where

import Control.Arrow ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, put)
import Control.Monad.Writer (tell)
import Data.Attoparsec.ByteString (Parser, choice, many1, parseOnly)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Function (fix)
import Data.Word (Word8)
import Freer.Impl (Freer, genFreer, interpretM, singleton)
import Freer.State (StateT, evalStateT)
import Freer.Writer (Writer, runWriter)
import Memory (Memory, center, emptyMemory, mapCenter, move)
import System.IO (hFlush, stdout)

type BFProgram a = Freer BF a

data BF a where
  Add :: Int -> BF ()
  Mov :: Int -> BF ()
  GetC :: BF ()
  PutC :: BF ()
  Loop :: BFProgram a -> BF ()

lexerBF :: Parser [BF ()]
lexerBF = many1 bf
  where
    bf =
      choice
        [ Add 1 <$ char '+',
          Add (-1) <$ char '-',
          Mov 1 <$ char '>',
          Mov (-1) <$ char '<',
          GetC <$ char ',',
          PutC <$ char '.',
          Loop <$> loopB
        ]
    loopB = genFreer <$> (char '[' *> many1 bf <* char ']')

interpretBF :: BFProgram r -> StateT Memory IO r
interpretBF = interpretM $ \case
  Add n -> modify $! mapCenter (+ fromIntegral n)
  Mov n -> modify $! move n
  GetC -> liftIO getChar >>= (toWord8 >>> const >>> mapCenter >>> modify)
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
  where
    toWord8 :: Char -> Word8
    toWord8 = fromEnum >>> fromIntegral

logBF :: BFProgram a -> Writer ByteString a
logBF = interpretM $ \case
  Add n -> tell $ if n >= 0 then B.replicate n '+' else B.replicate n '-'
  Mov n -> tell $ if n >= 0 then B.replicate n '>' else B.replicate n '<'
  GetC -> tell ","
  PutC -> tell "."
  Loop bf -> tell "[" >> logBF bf >> tell "]"

translateC :: BFProgram a -> ByteString
translateC = writerC >>> runWriter >>> snd >>> capturer
  where
    writerC :: BFProgram a -> Writer ByteString a
    writerC = interpretM $ \case
      Add n -> tell . B.pack $! "*p+=" <> show n
      Mov n -> tell . B.pack $! "p+=" <> show n
      GetC -> tell "getchar(*p);"
      PutC -> tell "putchar(*p);"
      Loop bf -> tell "while(*p){" >> writerC bf >> tell "}"
    capturer = (header <>) . (<> end)
    header = B.unlines ["#include <stdio.h>", "int main(){int m[30000]={};int *p=m;"]
    end = "}"

composeBFCode :: BFProgram a -> ByteString
composeBFCode = snd . runWriter . logBF

runBF :: BFProgram a -> IO a
runBF = flip evalStateT (emptyMemory 30000) . interpretBF

formatCode :: ByteString -> ByteString
formatCode = B.filter (`B.elem` "+-><,.[]")

transformCode :: (String -> a) -> (BFProgram () -> a) -> ByteString -> a
transformCode l r code = case parseOnly lexerBF (formatCode code) of
  Left err -> l err
  Right pg -> r $ genFreer pg

evalBFCode :: ByteString -> IO ()
evalBFCode = transformCode print runBF

genCProgram :: ByteString -> ByteString
genCProgram = transformCode error translateC

inc, dec, next, prev, getC, putC :: BFProgram ()
inc = singleton $ Add 1
dec = singleton $ Add (-1)
next = singleton $ Mov 1
prev = singleton $ Mov (-1)
getC = singleton GetC
putC = singleton PutC

add, mov :: Int -> BFProgram ()
add = singleton . Add
mov = singleton . Mov

loop :: BFProgram a -> BFProgram ()
loop = singleton . Loop
