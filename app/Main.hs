{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Freer.Impl

main :: IO ()
main = runConsole $ do
  putC "test"
  x <- getC
  putC $ "Hello " <> x

data Console r where
  PutC :: String -> Console ()
  GetC :: Console String

putC :: String -> Freer Console ()
putC = singleton . PutC

getC :: Freer Console String
getC = singleton GetC

runConsole :: Freer Console r -> IO r
runConsole = interpretM $ \case
  PutC str -> putStrLn str
  GetC -> getLine
