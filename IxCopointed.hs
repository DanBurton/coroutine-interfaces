{-# LANGUAGE Rank2Types, LambdaCase, InstanceSigs, KindSignatures, ScopedTypeVariables, DefaultSignatures, ConstraintKinds #-}

import Prelude hiding (map, (.), id)
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Category
import Control.Arrow

newtype Producing o i m r
  = Producing { resume :: m (ProducerState o i m r) }

data ProducerState o i m r
  = Produced o (Consuming r m i o)
  | Done r

newtype Consuming r m i o
  = Consuming { provide :: i -> Producing o i m r }

----------------------------------------------------------------

instance (Monad m) => Functor (Producing o i m) where
   fmap = liftM

instance (Monad m) => Applicative (Producing o i m) where
   pure = return
   (<*>) = ap

instance (Monad m) => Monad (Producing o i m) where
   return x = Producing $ return (Done x)
   p >>= f = Producing $ resume p >>= \s -> case s of
     Done x -> resume (f x)
     Produced o k ->
      return $ Produced o $ Consuming (provide k >=> f)

instance MonadTrans (Producing o i) where
  lift m = Producing (liftM Done m)

---------------------------------------------------------------

instance (Monad m) => Functor (Consuming r m a) where
  fmap f = go where
    go p = Consuming $ rewrap p >=> \s -> case s of
      Done r -> return r
      Produced a k ->
        yield (f a) >>= provide (go k)

instance (Monad m) => Applicative (Consuming r m a) where
  pure a = arr (const a)
  pf <*> px = Consuming $ \a -> rewrap pf a >>= \s -> case s of
    Done r -> return r
    Produced f pf' -> rewrap px a >>= \s -> case s of
      Done r -> return r
      Produced x px' ->
        yield (f x) >>= provide (pf' <*> px')

instance (Monad m) => Category (Consuming r m) where
  id = echo
  (.) = flip fuse

instance (Monad m) => Arrow (Consuming r m) where
  arr f = Consuming go where go = yield . f >=> go
  first k = Consuming $ \(b, d) -> rewrap k b >>= \s -> case s of
    Done r -> return r
    Produced c k' -> yield (c, d) >>= provide (first k')

---------------------------------------------------------------

yield :: Monad m => o -> Producing o i m i
yield o = Producing $ return $ Produced o $ Consuming return

echo :: Monad m => Consuming r m a a
echo = Consuming go where go = yield >=> go


rewrap :: (MonadTrans t, Monad m) =>
  Consuming r m i o -> i -> t m (ProducerState o i m r)
rewrap p a = lift (resume (provide p a))

fuse :: Monad m => Consuming r m a b -> Consuming r m b c -> Consuming r m a c
fuse p1 p2 = Consuming $ rewrap p1 >=> \s -> case s of
  Done r -> return r
  Produced b p1' -> rewrap p2 b >>= \s -> case s of
    Done r -> return r
    Produced c p2' ->
      yield c >>= provide (fuse p1' p2')

lift2 :: (MonadTrans t, MonadTrans t2, Monad m, Monad (t m)) =>
  m a -> t2 (t m) a
lift2 m = lift (lift m)

-----------------------------------------------------------------

instance IxMFunctor Producing where
  map f = go where
    go p = Producing $ f $ liftM map' (resume p)
    map' (Done r) = Done r
    map' (Produced o k) = Produced o $ Consuming (go . provide k)


class IxMPointed t where
  inject :: Monad m => m a -> t i i m a

instance IxMPointed Producing where
  inject = lift


instance IxMCopointed Producing where
  extract p = resume p >>= \case
    Done r -> return r
    Produced o k -> extract (provide k o)

type IxMTrans t = forall i j m. Monad m => Monad (t i j m)

class IxMFunctor t where
  map :: (Monad m, Monad m') => (forall x. m x -> m' x) -> t j k m a -> t j k m' a

class IxMCopointed t where
  extract :: Monad m => t i i m a -> m a

class (IxMFunctor t, IxMCopointed t) => IxMComonad t where
  extend :: (Monad m, Monad m') => (forall x. t j k m x -> m' x) -> t i k m a -> t i j m' a

  duplicate :: (Monad m) => t i k m a -> t i j (t j k m) a
  default duplicate :: (Monad m, Monad (t j k m)) => t i k m a -> t i j (t j k m) a
  duplicate = extend id

instance IxMComonad Producing where
  extend :: forall i k (m :: * -> *) r j (m' :: * -> *). (Monad m, Monad m') =>
    (forall x. Producing j k m x -> m' x) ->
    Producing i k m r -> Producing i j m' r
  extend morph = go where
    go :: Producing i k m r -> Producing i j m' r
    go p = Producing $ morph $ liftM map' (lift (resume p))
    map' (Done (r :: r)) = Done r
    map' (Produced (i :: i) (consuming :: Consuming r m k i)) =
        Produced i $ Consuming $ \(j :: j) -> do
          (k :: k) <- lift (jToK j)
          go (provide consuming k)
    -- the trick is to realize that the m' monad
    -- can turn j's into k's
    jToK :: j -> m' k
    jToK j = morph (yield j)

