{-# LANGUAGE Rank2Types, DefaultSignatures #-}

module Control.IxM (
    Functor(..),

    Pointed(..),
    Monad(..),

    Copointed(..),
    Comonad(..)
  ) where

import qualified Prelude as P (Monad, id)


class Functor t where
  map :: (P.Monad m, P.Monad m') =>
    (forall x. m x -> m' x) -> t j k m a -> t j k m' a
  -- map = hoist


class Pointed t where
  return :: P.Monad m => m a -> t i i m a
  -- return = lift

class (Functor t) => Monad t where
  bind :: (P.Monad m, P.Monad m') =>
    (forall x. m x -> t j k m' x) -> t i j m a -> t i k m' a
  default bind :: (P.Monad m, P.Monad m', P.Monad (t j k m')) =>
    (forall x. m x -> t j k m' x) -> t i j m a -> t i k m' a
  bind f m = join (map f m)

  join :: P.Monad m => t i j (t j k m) a -> t i k m a
  default join :: (P.Monad m, P.Monad (t j k m)) => t i j (t j k m) a -> t i k m a
  join = bind P.id


class Copointed t where
  extract :: P.Monad m => t i i m a -> m a

class (Functor t) => Comonad t where
  extend :: (P.Monad m, P.Monad m') =>
    (forall x. t j k m x -> m' x) -> t i k m a -> t i j m' a
  default extend :: (P.Monad m, P.Monad m', P.Monad (t j k m)) =>
    (forall x. t j k m x -> m' x) -> t i k m a -> t i j m' a
  extend f m = map f (duplicate m)

  duplicate :: (P.Monad m) => t i k m a -> t i j (t j k m) a
  default duplicate :: (P.Monad m, P.Monad (t j k m)) => t i k m a -> t i j (t j k m) a
  duplicate = extend P.id

