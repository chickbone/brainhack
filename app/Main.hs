{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brainfuck
import qualified Data.ByteString as B
import System.Environment (getArgs)

main :: IO ()
main = do
  filename <- fmap (!!0) getArgs
  B.readFile filename >>= B.putStr . genCProgram
