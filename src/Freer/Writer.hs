{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Freer.Writer where

import Control.Monad.Reader (MonadIO (..), MonadReader (ask, local), MonadTrans (..))
import Control.Monad.State.Strict (MonadState (get, put))
import Control.Monad.Writer (MonadWriter (listen, pass, tell, writer))
import Data.Functor.Identity (Identity (runIdentity))

type Writer w = WriterT w Identity

runWriter :: Writer w a -> (a, w)
runWriter ma = runIdentity $ runWriterT ma
{-# INLINEABLE runWriter #-}

evalWriter :: Writer w a -> w
evalWriter ma = runIdentity $ evalWriterT ma
{-# INLINEABLE evalWriter #-}

------------------------------

-- | CPS WriterT
newtype WriterT w m a = WriterT {unWriterT :: forall r. (a -> w -> m r) -> m r}

runWriterT :: Monad m => WriterT w m a -> m (a, w)
runWriterT (WriterT ma) = ma (curry pure)
{-# INLINEABLE runWriterT #-}

evalWriterT :: Monad m => WriterT w m a -> m w
evalWriterT (WriterT ma) = ma (const pure)
{-# INLINEABLE evalWriterT #-}

instance Functor (WriterT w m) where
  fmap f (WriterT a) = WriterT $ \q -> a (q . f)
  {-# INLINEABLE fmap #-}

instance Monoid w => Applicative (WriterT w m) where
  pure a = WriterT $ \q -> q a mempty
  {-# INLINEABLE pure #-}
  WriterT mf <*> WriterT ma = WriterT $ \q -> mf $ \f !w' -> ma $ \a !w -> q (f a) $! w' <> w
  {-# INLINEABLE (<*>) #-}

instance Monoid w => Monad (WriterT w m) where
  WriterT ma >>= f = WriterT $ \q -> ma $ \a !w' -> unWriterT (f a) $ \b !w -> q b $! w' <> w
  {-# INLINEABLE (>>=) #-}

instance Monoid w => MonadTrans (WriterT w) where
  lift m = WriterT $ \p -> m >>= (`p` mempty)
  {-# INLINEABLE lift #-}

instance Monoid w => MonadWriter w (WriterT w m) where
  writer (a, w) = WriterT $ \c -> c a w
  {-# INLINEABLE writer #-}
  tell w = WriterT $ \c -> c () w
  {-# INLINEABLE tell #-}
  listen m = WriterT $ \c -> unWriterT m (\a w -> c (a, w) w)
  {-# INLINEABLE listen #-}
  pass m = WriterT $ \c -> unWriterT m (\(a, f) w -> c a (f w))
  {-# INLINEABLE pass #-}

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  get = lift get
  {-# INLINEABLE get #-}
  put = lift . put
  {-# INLINEABLE put #-}

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  ask = lift ask
  local f m = WriterT $ \c -> local f (runWriterT m) >>= uncurry c

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
  liftIO = lift . liftIO
  {-# INLINEABLE liftIO #-}
