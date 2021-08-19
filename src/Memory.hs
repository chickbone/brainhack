{-# LANGUAGE TypeFamilies #-}

module Memory where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)

data Memory = Memory ByteString Word8 ByteString
  deriving (Eq, Show)

class MonoFunctor a where
  type Element a
  omap :: (Element a -> Element a) -> a -> a

instance MonoFunctor ByteString where
  type Element ByteString = Word8
  omap = B.map

instance MonoFunctor Memory where
  type Element Memory = Word8
  omap f (Memory ls c rs) = Memory (omap f ls) (f c) (omap f rs)
