{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Freer.Writer where

newtype Writer w a = Writer {unWriter :: forall r. (a -> w -> r) -> r}

runWriter :: Writer w a -> (a, w)
runWriter (Writer ma) = ma (,)
{-# INLINE runWriter #-}

evalWriter :: Writer w a -> w
evalWriter (Writer ma) = ma (const id)
{-# INLINE evalWriter #-}

instance Functor (Writer w) where
  fmap f (Writer a) = Writer $ \q -> a (q . f)
  {-# INLINE fmap #-}

instance Monoid w => Applicative (Writer w) where
  pure a = Writer $ \q -> q a mempty
  {-# INLINE pure #-}
  Writer mf <*> Writer ma = Writer $ \q -> mf $ \f !w' -> ma $ \a !w -> q (f a) $! w' <> w
  {-# INLINE (<*>) #-}

instance Monoid w => Monad (Writer w) where
  Writer ma >>= f = Writer $ \q -> ma $ \a !w' -> unWriter (f a) $ \b !w -> q b $! w' <> w
  {-# INLINE (>>=) #-}

newtype WriterT w m a = WriterT {unWriterT :: forall r. (a -> w -> m r) -> m r}

runWriterT :: Monad m => WriterT w m a -> m (a, w)
runWriterT (WriterT ma) = ma (curry pure)
{-# INLINE runWriterT #-}

evalWriterT :: Monad m => WriterT w m a -> m w
evalWriterT (WriterT ma) = ma (const pure)
{-# INLINE evalWriterT #-}

instance Functor (WriterT w m) where
  fmap f (WriterT a) = WriterT $ \q -> a (q . f)
  {-# INLINE fmap #-}

instance Monoid w => Applicative (WriterT w m) where
  pure a = WriterT $ \q -> q a mempty
  {-# INLINE pure #-}
  WriterT mf <*> WriterT ma = WriterT $ \q -> mf $ \f !w' -> ma $ \a !w -> q (f a) $! w' <> w
  {-# INLINE (<*>) #-}

instance Monoid w => Monad (WriterT w m) where
  WriterT ma >>= f = WriterT $ \q -> ma $ \a !w' -> unWriterT (f a) $ \b !w -> q b $! w' <> w
  {-# INLINE (>>=) #-}
