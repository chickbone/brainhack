{-# LANGUAGE TypeFamilies #-}

module Memory where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.MonoTraversable
import Data.Word (Word8)

data Memory = Memory ByteString Word8 ByteString
  deriving (Eq, Show)

type instance Element Memory = Word8

instance MonoFunctor Memory where
  omap f (Memory ls c rs) = Memory (omap f ls) (f c) (omap f rs)

emptyMemory :: Int -> Memory
emptyMemory l = Memory B.empty 0 $ B.replicate (l - 1) 0

center :: Memory -> Word8
center (Memory _ c _) = c

mapCenter :: (Word8 -> Word8) -> Memory -> Memory
mapCenter f (Memory l c r) = Memory l (f c) r

left, right :: Memory -> Memory
left (Memory ls c rs) = Memory (B.singleton c <> ls) (B.head rs) (B.tail rs)
right (Memory ls c rs) = Memory (B.tail ls) (B.head ls) (B.singleton c <> rs)
