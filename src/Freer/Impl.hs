{-# LANGUAGE RankNTypes #-}

module Freer.Impl (Freer (), FreerT (), interpret, interpretM, interpretT, singleton) where

import Control.Monad (join)
import Control.Monad.Trans.Class
import Data.Functor.Identity

newtype Freer t a = Freer {unFreer :: forall r. (a -> r) -> (forall x. t x -> (x -> r) -> r) -> r}

instance Functor (Freer t) where
  fmap f (Freer m) = Freer $ \p -> m (p . f)

instance Applicative (Freer t) where
  pure a = Freer $ \p _ -> p a
  Freer mf <*> Freer ma = Freer $ \p r -> mf (\f -> ma (p . f) r) r

instance Monad (Freer t) where
  Freer ma >>= f = Freer $ \p r -> ma (\a -> unFreer (f a) p r) r

interpret :: (forall x. t x -> x) -> Freer t a -> a
interpret k f = runIdentity (interpretM (Identity . k) f)

interpretM :: Monad m => (forall x. t x -> m x) -> Freer t a -> m a
interpretM k (Freer m) = m pure (\t p -> k t >>= p)

singleton :: t a -> Freer t a
singleton t = Freer $ \p r -> r t p

newtype FreerT t m a = FreerT {unFreerT :: forall r. (a -> r) -> (m r -> r) -> (forall x. t x -> (x -> r) -> r) -> r}

instance Functor (FreerT t m) where
  fmap f (FreerT m) = FreerT $ \p -> m (p . f)

instance Applicative (FreerT t m) where
  pure a = FreerT $ \p _ _ -> p a
  FreerT mf <*> FreerT ma = FreerT $ \p e r -> mf (\f -> ma (p . f) e r) e r

instance Monad (FreerT t m) where
  FreerT ma >>= f = FreerT $ \p e r -> ma (\a -> unFreerT (f a) p e r) e r

interpretT :: Monad m => (forall x. t x -> m x) -> FreerT t m a -> m a
interpretT k (FreerT m) = m pure join (\t p -> k t >>= p)

instance MonadTrans (FreerT t) where
  lift m = FreerT $ \p e _ -> e (fmap p m)
