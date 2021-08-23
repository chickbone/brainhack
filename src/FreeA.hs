{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module FreeA where

import Control.Arrow
import Control.Category hiding (id, (.))
import qualified Control.Category as Cat

newtype A f a b = Arr {unA :: forall r. Arrow r => (forall x y. f x y -> r x y) -> r a b}

instance Category (A f) where
  id = Arr $ \_ -> Cat.id
  Arr g . Arr f = Arr $ \k -> f k >>> g k

instance Arrow (A f) where
  arr f = Arr $ \_ -> arr f
  first (Arr f) = Arr $ \k -> first (f k)
  second (Arr f) = Arr $ \k -> second (f k)
  Arr f *** Arr g = Arr $ \k -> f k *** g k
  Arr f &&& Arr g = Arr $ \k -> f k &&& g k

interptetA :: Arrow r => (forall x y. f x y -> r x y) -> A f a b -> r a b
interptetA k (Arr g) = g k

interptetF :: (forall x y. f x y -> (->) x y) -> A f a b -> (->) a b
interptetF = interptetA

data LogicalGate i o where
  Not :: LogicalGate Bool Bool
  And :: LogicalGate (Bool, Bool) Bool
  Or :: LogicalGate (Bool, Bool) Bool

runGate :: A LogicalGate a b -> a -> b
runGate = interptetF $ \case
  Not -> not
  And -> uncurry (&&)
  Or -> uncurry (||)
