{-# LANGUAGE RankNTypes #-}

module Freer.State where

import Data.Functor.Identity

type State s = StateT s Identity

runState :: State s a -> s -> (a, s)
runState ma = runIdentity . runStateT ma

evalState :: State s a -> s -> a
evalState ma = runIdentity . evalStateT ma

execState :: State s a -> s -> s
execState ma = runIdentity . execStateT ma

newtype StateT s m a = StateT {unStateT :: forall r. s -> (a -> s -> m r) -> m r}

runStateT :: Monad m => StateT s m a -> s -> m (a, s)
runStateT (StateT ma) s = ma s (curry pure)

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT (StateT ma) s = ma s $ \a _ -> pure a

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT (StateT ma) s = ma s (const pure)

instance Functor (StateT s m) where
  fmap f (StateT ma) = StateT $ \s p -> ma s (p . f)
  {-# INLINEABLE fmap #-}

instance Applicative (StateT s m) where
  pure a = StateT $ \s p -> p a s
  {-# INLINEABLE pure #-}
  StateT mf <*> StateT ma = StateT $ \s p -> mf s $ \f s' -> ma s' (p . f)
  {-# INLINEABLE (<*>) #-}

instance Monad (StateT s m) where
  StateT ma >>= f = StateT $ \s p -> ma s $ \a s' -> unStateT (f a) s' p
  {-# INLINEABLE (>>=) #-}
