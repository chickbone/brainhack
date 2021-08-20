{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module FreeA where

import Control.Category hiding (id ,(.))
import qualified Control.Category as Cat
import Control.Arrow
import Data.Kind (Type)

newtype A f a b = Arr {unA :: forall r. Arrow r => (f ~>> r) -> r a b}

instance Category (A f) where
  id = Arr $ \ _ -> Cat.id
  Arr g . Arr f = Arr $ \p -> f p >>> g p

instance Arrow (A f) where
  arr f = Arr $ \ _ -> arr f
  Arr f *** Arr g = Arr $ \k -> f k *** g k

type (~>>) (f :: k -> j -> Type) (g :: k -> j -> Type) = forall (x :: k) (y :: j). f x y -> g x y 
