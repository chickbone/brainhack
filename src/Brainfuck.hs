{-# LANGUAGE GADTs,OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Brainfuck where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify, put)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.Attoparsec.ByteString (Parser, choice, many1, parseOnly)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Mason.Builder
import Data.Function (fix)
import Freer.Impl (Freer, interpretM, singleton, genFreer)
import Memory (Memory, center, emptyMemory, left, mapCenter, right)
import System.IO (stdout, hFlush)

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
  Inc -> modify $ mapCenter (+ 1)
  Dec -> modify $ mapCenter (subtract 1)
  Next -> modify left
  Prev -> modify right
  GetC -> liftIO getChar >>= modify . mapCenter . const . fromIntegral . fromEnum
  PutC -> do
    mem <- get
    let c = center mem
    liftIO . putChar . toEnum . fromIntegral $ c
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
translateC = toStrictByteString . snd . runWriter . writerC
  where
    writerC :: BFProgram a -> Writer (BuilderFor StrictByteStringBackend) a
    writerC = interpretM $ \case
      Inc -> tellBuilder "(*p)++;"
      Dec -> tellBuilder "(*p)--;"
      Next -> tellBuilder "++p;"
      Prev -> tellBuilder "--p;"
      GetC -> tellBuilder "*p=getchar();"
      PutC -> tellBuilder "putchar(*p);"
      Loop bf -> tellBuilder "while(*p){" >> writerC bf >> tellBuilder "}"

tellBuilder :: ByteString -> Writer (BuilderFor StrictByteStringBackend) ()
tellBuilder = tell . byteString

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

inc, dec, next, prev, getC, putC :: BFProgram ()
inc = singleton Inc
dec = singleton Dec
next = singleton Next
prev = singleton Prev
getC = singleton GetC
putC = singleton PutC

loop :: BFProgram a  -> BFProgram ()
loop = singleton . Loop
