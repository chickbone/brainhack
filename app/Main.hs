{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brainfuck
import Control.Monad

main :: IO ()
main = runBF do
  add 5
  next
  add 3
  prev
  loop do
    dec
    next
    loop do
      dec
      move 2
      inc
      move (-2)
    move 3
    loop do
      dec
      prev
      inc
      prev
      inc
      move 2
    move (-3)
  move 2
  putC

add :: Int -> BFProgram ()
add = flip replicateM_ inc

move :: Int -> BFProgram ()
move 0 = pure ()
move n
  | n > 0 = replicateM_ n next
  | otherwise = replicateM_ (abs n) prev
