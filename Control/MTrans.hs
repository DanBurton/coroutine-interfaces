{-# LANGUAGE Rank2Types, DefaultSignatures #-}

module Control.MTrans (
    Functor(..),
    Monad(..),
    Copointed(..),
    Comonad(..)
  ) where

import qualified Prelude as P (Monad, id)
import Control.Monad.Trans.Class (MonadTrans, lift)

class (MonadTrans t) => Functor t where
  map :: (P.Monad m, P.Monad m') =>
    (forall x. m x -> m' x) -> t m a -> t m' a

-- Pointed = MonadTrans, is superfluous

class (Functor t) => Monad t where
  bind :: (P.Monad m, P.Monad m') =>
    (forall x. m x -> t m' x) -> t m a -> t m' a
  default bind :: (P.Monad m, P.Monad m', P.Monad (t m')) =>
    (forall x. m x -> t m' x) -> t m a -> t m' a
  bind f m = join (map f m)

  join :: P.Monad m => t (t m) a -> t m a
  default join :: (P.Monad m, P.Monad (t m)) => t (t m) a -> t m a
  join = bind P.id


class Copointed t where
  extract :: P.Monad m => t m a -> m a

class (Functor t) => Comonad t where
  extend :: (P.Monad m, P.Monad m') =>
    (forall x. t m x -> m' x) -> t m a -> t m' a
  extend f m = lift (f m)
  -- default extend :: (P.Monad m, P.Monad m', P.Monad (t m)) =>
  --   (forall x. t m x -> m' x) -> t m a -> t m' a
  -- extend f m = map f (duplicate m)

  duplicate :: (P.Monad m) => t m a -> t (t m) a
  default duplicate :: (P.Monad m, P.Monad (t m)) => t m a -> t (t m) a
  duplicate = extend P.id

