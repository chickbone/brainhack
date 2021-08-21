{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brainfuck

main :: IO ()
main = evalBFCode "+++++++++++++++++++++++++++++++++++++++++++.-.++.-."
