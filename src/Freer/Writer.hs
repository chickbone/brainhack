{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Freer.Writer where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans, lift)
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

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
  liftIO = lift . liftIO
  {-# INLINEABLE liftIO #-}
