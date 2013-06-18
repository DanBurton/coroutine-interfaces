{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- Taken from monad-coroutine
newtype Coroutine s m r
  = Coroutine { resume :: m (CoroutineState s m r) }

data CoroutineState s m r
  = Run (s (Coroutine s m r))
  | Done r

instance (Functor s, Functor m) => Functor (Coroutine s m) where
   fmap f t = Coroutine (fmap (apply f) (resume t))
      where apply fc (Done x) = Done (fc x)
            apply fc (Run s) = Run (fmap (fmap fc) s)

instance (Functor s, Functor m, Monad m) => Applicative (Coroutine s m) where
   pure = return
   (<*>) = ap

instance (Functor s, Monad m) => Monad (Coroutine s m) where
   return x = Coroutine (return (Done x))
   t >>= f = Coroutine (resume t >>= apply f)
      where apply fc (Done x) = resume (fc x)
            apply fc (Run s) = return (Run (fmap (>>= fc) s))
   t >> f = Coroutine (resume t >>= apply f)
      where apply fc (Done x) = resume fc
            apply fc (Run s) = return (Run (fmap (>> fc) s))

instance Functor s => MonadTrans (Coroutine s) where
   lift = Coroutine . liftM Done

instance (Functor s, MonadIO m) => MonadIO (Coroutine s m) where
   liftIO = lift . liftIO

hoist :: forall s m m' x. (Functor s, Monad m, Monad m') =>
            (forall y. m y -> m' y) -> Coroutine s m x -> Coroutine s m' x
hoist f cort = Coroutine {resume= liftM map' (f $ resume cort)}
   where map' (Done r) = Done r
         map' (Run s) = Run (fmap (hoist f) s)


suspend :: (Monad m, Functor s) => s (Coroutine s m x) -> Coroutine s m x
suspend s = Coroutine (return (Run s))

newtype PauseF x = PauseF x
instance Functor PauseF where fmap f (PauseF x) = PauseF (f x)
type PauseT = Coroutine PauseF

pause :: Monad m => PauseT m ()
pause = suspend $ PauseF (return ())

type PauseF' = Interface () ()
type PauseT' = Coroutine PauseF'

pause' :: Monad m => PauseT' m ()
pause' = suspend $ Produced () (\() -> return ())

data Interface i o x
  = Produced o (i -> x)

instance Functor (Interface i o) where
  fmap f (Produced o k) = Produced o (f . k)


yield :: Monad m => o -> Producing o i m i
yield o = suspend $ Produced o return

type Producing o i = Coroutine (Interface i o)
type Consuming r m i o = i -> Producing o i m r

foreverK :: Monad m => (a -> m a) -> a -> m r
foreverK f = go where
  go a = f a >>= go

echo :: Monad m => Consuming r m a a
echo = arr id

arr :: Monad m => (a -> b) -> Consuming r m a b
arr f = foreverK (yield . f)

example1 :: Producing String String IO ()
example1 = do
  line1 <- prompt "What's your name? "
  liftIO $ putStrLn $ "Hello, " ++ line1
  line2 <- prompt "What's your favorite color? "
  liftIO $ putStrLn $ "I like " ++ line2 ++ ", too."
  where prompt = yield

stdInOut :: Producing String String IO r
stdInOut = stdOutIn ""

stdOutIn :: Consuming r IO String String
stdOutIn = foreverK $ \str -> do
  liftIO $ putStr str
  liftIO getLine >>= yield

infix 8 $$
infixl 8 $=
infixr 8 =$

($$) :: Monad m => Producing a b m r -> Consuming r m a b -> m r
producing $$ consuming = resume producing >>= \case
  Done r -> return r
  Run (Produced o k) -> consuming o $$ k

type Proxy inD outD outU inU m r = Consuming r (Producing outU inU m) inD outD)

insert0 = lift
insert1 = hoist insert0
insert2 = hoist insert1

($=) :: Monad m => Producing a b m r -> Proxy a b a' b' m r -> Producing a' b' m r
producing $= proxy = insert1 producing $$ proxy

(=$) :: Monad m => Proxy a' b' a b m r -> Consuming r m a b -> Consuming r m a' b'
(proxy =$ consuming) a' = commute (proxy a') $$ insert1 . consuming

(=$=) :: forall a a' b b' c c' m r. Monad m => Proxy a a' b b' m r -> Proxy b b' c c' m r -> Proxy a a' c c' m r
(proxyl =$= proxyr) a = producerl' $$ proxyr' where
  producerl :: Producing b b' (Producing a' a m) r
  producerl = commute (proxyl a)
  producerl' :: Producing b b' (Producing a' a (Producing c c' m)) r
  producerl' = insert2 producerl
  proxyr' :: Proxy b b' a' a (Producing c c' m) r
  proxyr' = insert1 . proxyr

idPull :: Monad m => Proxy a b a b m r
idPull = foreverK $ lift . yield >=> yield

commute :: forall a b c d m r. Monad m => Producing a b (Producing c d m) r -> Producing c d (Producing a b m) r
commute p = p' $$ idP where
  p' :: Producing a b (Producing c d (Producing a b m)) r
  p' = insert2 p
  idP :: Proxy a b c d (Producing a b m) r
  idP = insert1 . idPull

fuse :: Monad m => Consuming r m a b -> Consuming r m b c -> Consuming r m a c
fuse p1 p2 a =lift (resume (p1 a)) >>= \case
    Done r -> return r
    Run (Produced (b :: b) p1') -> lift (resume (p2 b)) >>= \case
      Done r -> return r
      Run (Produced (c :: c) p2') -> yield c >>= fuse p1' p2'

