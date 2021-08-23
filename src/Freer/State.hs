{-# LANGUAGE RankNTypes #-}
module Freer.State where

-- data State s a = State {runState :: s -> a -> (a, s)}

newtype StateT s m a = StateT {unStateT :: forall r. s -> (a -> s -> m r) -> m r }

instance Functor (StateT s m) where
  fmap f (StateT ma) = StateT $ \s p -> ma s (p . f)

instance Applicative (StateT s m) where
  pure a = StateT $ \s p -> p a s
  StateT mf <*> StateT ma = StateT $ \s p -> mf s $ \f s' -> ma s' (p . f)

instance
