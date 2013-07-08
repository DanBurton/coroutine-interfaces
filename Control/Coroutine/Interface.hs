-- {-# LANGUAGE ScopedTypeVariables, InstanceSigs, KindSignatures #-}

module Control.Coroutine.Interface (
    Producing(..),
    Consuming(..),
    ProducerState(..),
    yield,
    overC,
    replaceYield,
    pfold,
    push,
    ($$),
    (+$+)
  ) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Function (fix)
import Data.Monoid
import Control.Util

import qualified Control.IxM as IxM
import qualified Control.MTrans as MT

newtype Producing o i m r
  = Producing { resume :: m (ProducerState o i m r) }

data ProducerState o i m r
  = Produced o (Consuming r m i o)
  | Done r

newtype Consuming r m i o
  = Consuming { provide :: i -> Producing o i m r }

type Resumable o i m r
  = Either (ProducerState i o m r) (ProducerState o i m r)

----------------------------------------------------------------


yield :: Monad m => o -> Producing o i m i
yield o = Producing $ return $ Produced o $ Consuming return

push :: Monad m => Producing o i m r -> Consuming r m i o
push p = Consuming $ \i -> Producing $ resume p >>= \s -> case s of
  Done r -> return (Done r)
  Produced o k -> return (Produced o (push (provide k i)))

push' :: Monad m => Producing o i m r -> Consuming (i, r) m i o
push' p = Consuming $ \i -> Producing $ resume p >>= \s -> case s of
  Done r -> return (Done (i, r))
  Produced o k -> return (Produced o (push' (provide k i)))

overC :: (Producing o i m r -> Producing o' i m' r') -> Consuming r m i o -> Consuming r' m' i o'
overC f k = Consuming (f . provide k)


pfold :: (Monad m') => (m (ProducerState o i m r) -> m' (ProducerState o i m r)) ->
  (r -> m' r') -> (o -> m' i) -> Producing o i m r -> m' r'
pfold morph return' yield' p0 = go p0 where
  go p = morph (resume p) >>= \s -> case s of
    Done r -> return' r
    Produced o k -> yield' o >>= go . provide k

-- pfold lift return yield === id


replaceYield :: (Monad m) => (o -> Producing o' i' m i) -> Producing o i m r -> Producing o' i' m r
replaceYield = pfold lift return



infixl 0 $$
($$) :: Monad m => Producing a b m r -> Consuming r m a b -> m r
producing $$ consuming = resume producing >>= \s -> case s of
  Done r -> return r
  Produced o k -> provide consuming o $$ k

(+$+) :: Monad m => Consuming r m b a -> Consuming r m a b -> b -> m (Resumable b a m r, r)
k1 +$+ k2 = \b -> resume (provide k1 b) >>= \s -> case s of
  Done r -> return (Right (Produced b k2), r)
  (Produced a k1') -> resume (provide k2 a) >>= \s2 -> case s2 of
    Done r -> return (Left (Produced a k1'), r)
    Produced b' k2' -> (k1' +$+ k2') b'

-- a common pattern in implementation
rewrap :: (MonadTrans t, Monad m) =>
  Consuming r m i o -> i -> t m (ProducerState o i m r)
rewrap p a = lift (resume (provide p a))

commute :: (Monad m) => Producing a b (Producing c d m) r -> Producing c d (Producing a b m) r
commute p = p' $$ idP where
  p' = insert2 p
  idP = insert1 `overC` idProxy
  idProxy = Consuming $ fix ((lift . yield >=> yield) >=>)


----------------------------------------------------------------

instance (Monad m) => Functor (Producing o i m) where
   fmap = liftM

instance (Monad m) => Applicative (Producing o i m) where
   pure = return
   (<*>) = ap

instance (Monad m) => Monad (Producing o i m) where
   return x = lift (return x)
   p >>= f = Producing $ resume p >>= \s -> case s of
     Done r -> resume (f r)
     Produced o k -> return $ Produced o $ Consuming (provide k >=> f)
   -- if not for the circular definition...
   -- pfold lift f yield p

instance MonadTrans (Producing o i) where
  lift m = Producing (liftM Done m)

---------------------------------------------------------------

instance (Monad m) => Functor (Consuming r m a) where
  fmap f = overC $ replaceYield (yield . f)


instance (Monad m) => Applicative (Consuming r m a) where
  pure a = arr (const a)
  kf <*> kx = Consuming $ \a -> rewrap kf a >>= \s -> case s of
    Done r -> return r
    Produced f kf' -> rewrap kx a >>= \s -> case s of
      Done r -> return r
      Produced x kx' ->
        yield (f x) >>= provide (kf' <*> kx')

arrowFmap :: (Arrow a) => (y -> z) -> a x y -> a x z
arrowFmap f a = arr f . a

arrowPure :: (Arrow a) => z -> a x z
arrowPure z = arr (const z)

arrowAp :: (Arrow a) => a b (x -> y) -> a b x -> a b y
arrowAp af ax = arrowFmap (uncurry ($)) (af &&& ax)

zipC :: Monad m => Consuming r m a b -> Consuming r m a' b' -> Consuming r m (a,a') (b,b')
zipC = (***)

instance (Monad m) => Category (Consuming r m) where
  id = Consuming $ fix (yield >=>)
  k2 . k1 = Consuming $ rewrap k1 >=> \s -> case s of
    Done r -> return r
    Produced b k1' -> rewrap k2 b >>= \s2 -> case s2 of
      Done r -> return r
      Produced c k2' ->
        yield c >>= provide (k2' . k1')

instance (Monad m) => Arrow (Consuming r m) where
  arr f = Consuming go where go = yield . f >=> go
  first k = Consuming $ \(b, d) -> rewrap k b >>= \s -> case s of
    Done r -> return r
    Produced c k' -> yield (c, d) >>= provide (first k')

instance (Monad m, Monoid r) => ArrowZero (Consuming r m) where
  zeroArrow = Consuming $ \_ -> return mempty

instance (Monad m, Monoid r) => ArrowPlus (Consuming r m) where
  k1 <+> k2 = Consuming $ \i -> rewrap k1 i >>= \s -> case s of
    Done r -> liftM (r <>) (provide k2 i)
    Produced o k1' -> yield o >>= provide (k1' <+> k2)

instance (Monad m) => ArrowChoice (Consuming r m) where
  left k = Consuming go where
    go = \e -> case e of
      Right d -> yield (Right d) >>= go
      Left b -> rewrap k b >>= \s -> case s of
        Done r -> return r
        Produced c k' -> yield (Left c) >>= provide (left k')
  -- left = leftApp ???

instance (Monad m) => ArrowApply (Consuming r m) where
  app = Consuming go where
    go = \(kf, b) -> rewrap kf b >>= \s -> case s of
      Done r -> return r
      Produced c _ -> yield c >>= go
      -- ignoring k' makes me sad

---------------------------------------------------------------

instance IxM.Functor Producing where
  map f = pfold (lift . f) return yield

    --go p = Producing $ f $ liftM map' (resume p)
    --map' (Done r) = Done r
    --map' (Produced o k) = Produced o $ Consuming (go . provide k)

instance IxM.Pointed Producing where
  return = lift

-- Producing is not an IxM.Monad. Consider join:
-- join :: Producing i j (Producing j k) m r -> Producing i k m r
-- This presumes to hook up the inner Producing's j outputs
-- with the outer Producing's j inputs.

-- This won't work when the two interfaces are not suspended upon
-- the same number of times.


instance IxM.Copointed Producing where
  extract = pfold id return return

  --resume p >>= \s -> case s of
  --  Done r -> return r
  --  Produced o k -> IxM.extract (provide k o)

instance IxM.Comonad Producing where
  extend morph = go where
    go p = Producing $ morph $ liftM map' (lift (resume p))
    map' (Done r) = Done r
    map' (Produced i consuming) = Produced i $ Consuming $ \j -> do
      k <- lift (jToK j)
      go (provide consuming k)
    jToK j = morph (yield j)

--------------------------------------------------------------------

instance MT.Functor (Producing o i) where
  map = IxM.map

instance MT.Monad (Producing o i) where
  join = pfold id return yield 

  --resume p >>= \s -> case s of
  --  Done r -> return r
  --  Produced o k -> yield o >>= MT.join . provide k

