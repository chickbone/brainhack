{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Freer.Impl (Freer (), FreerT (), interpret, interpretM, interpretWithM, interpretT, singleton, singletonM, interpretWith,genFreer) where

import Control.Monad (join)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Kind (Type)
import Data.Foldable (traverse_)

newtype Freer t a = Freer {unFreer :: forall r. (a -> r) -> (forall x. t x -> (x -> r) -> r) -> r}

instance Functor (Freer t) where
  fmap f (Freer m) = Freer $ \p -> m (p . f)

instance Applicative (Freer t) where
  pure a = Freer $ \p _ -> p a
  Freer mf <*> Freer ma = Freer $ \p r -> mf (\f -> ma (p . f) r) r

instance Monad (Freer t) where
  Freer ma >>= f = Freer $ \p r -> ma (\a -> unFreer (f a) p r) r

interpret :: (forall x. t x -> x) -> Freer t a -> a
interpret k (Freer m) = m id (\t f -> f $ k t)

interpretWith :: (forall x. t x -> x) -> (a -> r) -> Freer t a -> r
interpretWith k f = f . interpret k
{-# INLINE interpretWith #-}

interpretM :: Monad m => (t ~> m) -> Freer t ~> m
interpretM k (Freer m) = m pure (\t p -> k t >>= p)

interpretWithM :: Monad m => forall r. (t ~> m) -> (a -> r) -> Freer t a -> m r
interpretWithM k f = fmap f . interpretM k
{-# INLINE interpretWithM #-}

singleton :: t ~> Freer t
singleton t = Freer $ \p r -> r t p

genFreer :: Traversable list => list (t a) -> Freer t ()
genFreer = traverse_ singleton 

newtype FreerT t m a = FreerT {unFreerT :: forall r. (a -> r) -> (m r -> r) -> (forall x. t x -> (x -> r) -> r) -> r}

instance Functor (FreerT t m) where
  fmap f (FreerT m) = FreerT $ \p -> m (p . f)

instance Applicative (FreerT t m) where
  pure a = FreerT $ \p _ _ -> p a
  FreerT mf <*> FreerT ma = FreerT $ \p e r -> mf (\f -> ma (p . f) e r) e r

instance Monad (FreerT t m) where
  FreerT ma >>= f = FreerT $ \p e r -> ma (\a -> unFreerT (f a) p e r) e r

singletonM :: t ~> FreerT t m
singletonM t = FreerT $ \p _ r -> r t p
{-# INLINE singletonM #-}

interpretT :: Monad m => (t ~> m) -> FreerT t m ~> m
interpretT k (FreerT m) = m pure join (\t p -> k t >>= p)

instance MonadTrans (FreerT t) where
  lift m = FreerT $ \p e _ -> e (fmap p m)

type (~>) (f :: k -> Type) (g :: k -> Type) = forall (x :: k). f x -> g x
