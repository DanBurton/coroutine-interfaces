{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow

newtype Producing o i m r
  = Producing { resume :: m (ProducerState o i m r) }

data ProducerState o i m r
  = Produced o (Consuming r m i o)
  | Done r

newtype Consuming r m i o
  = Consuming { provide :: i -> Producing o i m r }


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


-- The monad laws

-- Prove that: return x >>= f === f x
-- return x >>= f                         -- begin
-- Producing (return (Done x)) >>= f      -- def. return
-- Producing $ return (Done x) >>= \case  -- def. >>=
--   Done x -> resume (f x)
-- Producing (resume (f x))               -- base monad law
-- f x                                    -- Producing (resume m) = m


-- Prove that: m >>= return === m
-- m >>= return
-- Producing $ resume m >>= \case
--   Done x -> resume (return x)
--   Produced o k ->
--     return $ Produced o $ Consuming ((>>= return) . provide k)
-- Branch: Done x
-- Producing $ resume m >>= \Done x -> resume (return x)
-- Producing $ resume m >>= \Done x -> resume (Producing $ return (Done x))
-- Producing $ resume m >>= \Done x -> return (Done x)
-- Producing $ resume m >>= return
-- Branch: Producing o k
-- Producing $ resume m >>= \Produced o k ->
--   return (Produced o (Consuming (go . provide k)))
--   return (Produced o (Consuming (provide k >=> return)))

-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)


instance MonadTrans (Producing o i) where
   lift = Producing . liftM Done

-- lift (return x) = return x
-- lift (return x)
-- Producing (liftM Done (return x))
-- Producing (return (Done x))

-- lift . (f >=> g) = lift . g >=> lift . g
-- 

instance MFunctor (Producing o i) where
  hoist f = go where
    go p = Producing $ f $ liftM map' (resume p)
    map' (Done r) = Done r
    map' (Produced o k) = Produced o $ Consuming (go . provide k)

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

upgrade :: Monad m => Producing o i m r -> Producing o a (Producing a i m) r
upgrade p = lift2 (resume p) >>= \s -> case s of
  Done r -> return r
  Produced o k -> do
    yield o >>= lift . yield >>= upgrade . provide k

collapse :: Monad m => Producing o a (Producing a i m) r -> Producing o i m r
collapse p = undefined


iextend :: Monad m =>
  (Producing j k m a -> m b) -> Producing i k m a -> Producing i j m b
iextend conn p = do
  

{-
upgrade :: Monad m => Consuming r m i o -> Consuming r (Producing a i m) a o
upgrade k = Consuming $ \(a :: a) -> do
  (i :: i) <- lift (yield a)
  lift (rewrap k i) >>= \s -> case s of
    Done r -> return r
    Produced (o :: o) k' -> do
      yield o >>= provide (upgrade k')
-}
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
