{-# LANGUAGE ScopedTypeVariables #-} -- this comes in handy
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Morph (MFunctor, hoist)

data Producing o i (m :: * -> *) r -- don't rely on the internals of this
instance (Monad m) => Monad (Producing o i m) where
instance MonadTrans (Producing o i) where
instance MFunctor (Producing o i) where

newtype Consuming r m i o
  = Consuming { provide :: i -> Producing o i m r }

infixl 0 $$
($$) :: Monad m => Producing a b m r -> Consuming r m a b -> m r
producing $$ consuming = undefined -- just take this as a given

idProxy :: Monad m => Consuming r (Producing a b m) a b
idProxy = undefined -- take this as a given

insert0 :: Monad m =>
  m r -> Producing a b m r
insert0 = lift

insert1 :: (MFunctor t, Monad m) =>
  t m r -> t (Producing a b m) r
insert1 = hoist insert0

insert2 :: (MFunctor t, MFunctor t2, Monad m, Monad (t m)) =>
  t2 (t m) r -> t2 (t (Producing a b m)) r
insert2 = hoist insert1

commute :: forall a b c d m r. Monad m =>
  Producing a b (Producing c d m) r -> Producing c d (Producing a b m) r
commute p = p' $$ funnel where
  p' :: Producing a b (Producing c d (Producing a b m)) r
  p' = insert2 p
  funnel :: Consuming r (Producing c d (Producing a b m)) a b
  funnel = Consuming (insert1 . provide idProxy)
