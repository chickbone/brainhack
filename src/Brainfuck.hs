{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Brainfuck where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify, put)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Attoparsec.ByteString (Parser, choice, many1, parseOnly)
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Function (fix)
import Freer.Impl (Freer, interpretM, singleton)
import Memory (Memory, center, emptyMemory, left, mapCenter, right)

type BFProgram a = Freer BF a

data BF a where
  Inc :: BF ()
  Dec :: BF ()
  Next :: BF ()
  Prev :: BF ()
  GetC :: BF ()
  PutC :: BF ()
  Loop :: [BF a] -> BF ()

instance Show (BF a) where
  show Inc = "Inc"
  show Dec = "Dec"
  show Next = "Next"
  show Prev = "Prev"
  show GetC = "GetC"
  show PutC = "PutC"
  show (Loop pg) = "Loop " <> show pg

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
          Loop <$> loop
        ]
    loop = char '[' *> many1 bf <* char ']'

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
      else interpretBF (mapM_ singleton pg) >> put mem >> loop

logBF :: BFProgram a -> Writer String a
logBF = interpretM $ \case
  Inc -> tell "+"
  Dec -> tell "-"
  Next -> tell ">"
  Prev -> tell "<"
  GetC -> tell ","
  PutC -> tell "."
  Loop bf -> tell "[" >> logBF (mapM_ singleton bf) >> tell "]"

rebuildBF :: BFProgram a -> String
rebuildBF = snd . runWriter . logBF

runBF :: BFProgram a -> IO a
runBF = flip evalStateT (emptyMemory 30000) . interpretBF

evalBFCode :: ByteString -> IO ()
evalBFCode code = case parseOnly lexerBF (B.filter (`elem` "+-><,.[]") code) of
  Left err -> print err
  Right pg -> runBF . mapM_ singleton $ pg

inc, dec, next, prev, getC, putC :: BFProgram ()
inc = singleton Inc
dec = singleton Dec
next = singleton Next
prev = singleton Prev
getC = singleton GetC
putC = singleton GetC
