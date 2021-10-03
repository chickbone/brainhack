{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brainfuck (add, dec, inc, loop, mov, next, prev, putC, runBF)

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
      mov 2
      inc
      mov (-2)
    mov 3
    loop do
      dec
      prev
      inc
      prev
      inc
      mov 2
    mov (-3)
  mov 2
  putC
