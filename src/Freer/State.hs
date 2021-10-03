{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Freer.State where

import Control.Monad.Cont (MonadCont (callCC))
import Control.Monad.Reader (MonadFix (..), MonadIO (..), MonadReader (ask, local), MonadTrans (..))
import Control.Monad.State.Strict (MonadState, get, put, state)
import Data.Functor.Identity (Identity (runIdentity))

type State s = StateT s Identity

runState :: State s a -> s -> (a, s)
runState ma = runIdentity . runStateT ma
{-# INLINEABLE runState #-}

evalState :: State s a -> s -> a
evalState ma = runIdentity . evalStateT ma
{-# INLINEABLE evalState #-}

execState :: State s a -> s -> s
execState ma = runIdentity . execStateT ma
{-# INLINEABLE execState #-}

newtype StateT s m a = StateT {unStateT :: forall r. s -> (a -> s -> m r) -> m r}

runStateT :: Monad m => StateT s m a -> s -> m (a, s)
runStateT (StateT ma) s = ma s (curry pure)
{-# INLINEABLE runStateT #-}

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT (StateT ma) s = ma s $ \a _ -> pure a
{-# INLINEABLE evalStateT #-}

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT (StateT ma) s = ma s (const pure)
{-# INLINEABLE execStateT #-}

instance Functor (StateT s m) where
  fmap f (StateT ma) = StateT $ \s p -> ma s (p . f)
  {-# INLINEABLE fmap #-}

instance Applicative (StateT s m) where
  pure a = StateT $ \s p -> p a s
  {-# INLINEABLE pure #-}
  StateT mf <*> StateT ma = StateT $ \ !s p -> mf s $ \f !s' -> ma s' (p . f)
  {-# INLINEABLE (<*>) #-}

instance Monad (StateT s m) where
  StateT ma >>= f = StateT $ \ !s p -> ma s $ \a !s' -> unStateT (f a) s' p
  {-# INLINEABLE (>>=) #-}

instance MonadState s (StateT s m) where
  get = StateT $ \s c -> c s s
  {-# INLINEABLE get #-}
  put s = StateT $ \_ c -> c () s
  {-# INLINEABLE put #-}
  state f = StateT $ \s c -> uncurry c (f s)
  {-# INLINEABLE state #-}

instance MonadTrans (StateT s) where
  lift m = StateT $ \s c -> m >>= \a -> c a s
  {-# INLINEABLE lift #-}

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance MonadReader e m => MonadReader e (StateT s m) where
  ask = lift ask
  local f m = stateT $ \s -> local f (runStateT m s)

instance MonadFix m => MonadFix (StateT s m) where
  mfix f = stateT $ \s -> mfix $ \ ~(a, _) -> runStateT (f a) s

instance MonadCont m => MonadCont (StateT s m) where
  callCC f = stateT $ \s -> callCC $ \c -> runStateT (f (\a -> stateT $ \s' -> c (a, s'))) s

-- A stricter version of 'state'. The latter uses the
-- lazy 'uncurry' function for some reason.
stateT :: Monad m => (s -> m (a, s)) -> StateT s m a
stateT f = StateT $ \s c -> do
  (a, s') <- f s
  c a s'
{-# INLINE stateT #-}
