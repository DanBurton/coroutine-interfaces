{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Category
import Control.Arrow

newtype Producing o i m r
  = Producing { resume :: m (ProducerState o i m r) }

data ProducerState o i m r
  = Produced o (Consuming r m i o)
  | Done r

newtype Consuming r m i o = Consuming { provide :: i -> Producing o i m r }

instance (Functor m) => Functor (Producing o i m) where
   fmap f p = Producing (fmap (fmap f) (resume p))

instance (Functor m) => Functor (ProducerState o i m) where
  fmap f (Done x) = Done (f x)
  fmap f (Produced o k) = Produced o (Consuming (fmap f . provide k))

instance (Functor m, Monad m) => Applicative (Producing o i m) where
   pure = return
   (<*>) = ap

instance (Monad m) => Monad (Producing o i m) where
   return x = Producing (return (Done x))
   
   p >>= f = Producing $ resume p >>= \case
     Done x -> resume (f x)
     Produced o k ->
      return $ Produced o $ Consuming ((>>= f) . provide k)


instance MonadTrans (Producing o i) where
   lift = Producing . liftM Done

instance (MonadIO m) => MonadIO (Producing o i m) where
   liftIO = lift . liftIO

hoist :: forall o i m m' x. (Monad m, Monad m') =>
            (forall y. m y -> m' y) -> Producing o i m x -> Producing o i m' x
hoist f p = Producing $ liftM map' (f $ resume p)
   where map' (Done r) = Done r
         map' (Produced o k) = Produced o (Consuming (hoist f . provide k))


yield :: Monad m => o -> Producing o i m i
yield o = Producing $ return $ Produced o (Consuming return)

foreverK :: Monad m => (a -> m a) -> a -> m r
foreverK f = go where
  go a = f a >>= go

echo :: Monad m => Consuming r m a a
echo = arr id


example1 :: Producing String String IO ()
example1 = do
  line1 <- prompt "What's your name? "
  liftIO $ putStrLn $ "Hello, " ++ line1
  line2 <- prompt "What's your favorite color? "
  liftIO $ putStrLn $ "I like " ++ line2 ++ ", too."
  where prompt = yield

stdInOut :: Producing String String IO r
stdInOut = provide stdOutIn ""

stdOutIn :: Consuming r IO String String
stdOutIn = Consuming $ foreverK $ \str -> do
  liftIO $ putStr str
  liftIO getLine >>= yield


infix 8 $$

($$) :: Monad m => Producing a b m r -> Consuming r m a b -> m r
producing $$ consuming = resume producing >>= \case
  Done r -> return r
  Produced o k -> provide consuming o $$ k

newtype Proxy r m downI upI
  = Proxy { unProxy :: Consuming r (Producing (Fst upI) (Snd upI) m) (Fst downI) (Snd downI) }

type family Fst (xy :: (*,*)) :: *
type family Snd (xy :: (*,*)) :: *
type instance Fst '(x,y) = x
type instance Snd '(x,y) = y

{-
instance (Monad m) => Category (Proxy r m) where
  id = idPull
  (.) = flip (=$=)
-}

idPull :: Monad m => Proxy r m '(a,b) '(a,b)
idPull = Proxy $ Consuming $ foreverK $ lift . yield >=> yield


insert0 = lift
insert1 = hoist insert0
insert2 = hoist insert1

infixl 8 $=
infixr 8 =$


($=) :: Monad m => Producing a b m r -> Proxy r m '(a,b) '(a',b') -> Producing a' b' m r
producing $= Proxy proxy = insert1 producing $$ proxy

(=$) :: Monad m => Proxy r m '(a',b') '(a,b) -> Consuming r m a b -> Consuming r m a' b'
Proxy proxy =$ consuming = Consuming $ \a' ->
  commute (provide proxy a') $$ Consuming (insert1 . provide consuming)

(=$=) :: forall a a' b b' c c' m r. Monad m =>
  Proxy r m '(a,a') '(b,b') -> Proxy r m '(b,b') '(c,c') -> Proxy r m '(a,a') '(c,c')
Proxy proxyl =$= Proxy proxyr = Proxy $ Consuming $ \a ->
  let
    producerl :: Producing b b' (Producing a' a m) r
    producerl = commute (provide proxyl a)
    producerl' :: Producing b b' (Producing a' a (Producing c c' m)) r
    producerl' = insert2 producerl
    proxyr' :: Consuming r (Producing a' a (Producing c c' m)) b b'
    proxyr' = Consuming $ insert1 . provide proxyr
  in
    producerl' $$ proxyr' where

commute :: forall a b c d m r. Monad m =>
  Producing a b (Producing c d m) r -> Producing c d (Producing a b m) r
commute p = p' $$ idP where
  p' :: Producing a b (Producing c d (Producing a b m)) r
  p' = insert2 p
  idP :: Consuming r (Producing c d (Producing a b m)) a b
  idP = Consuming (insert1 . provide (unProxy idPull))


fuse :: Monad m => Consuming r m a b -> Consuming r m b c -> Consuming r m a c
fuse p1 p2 = Consuming $ \a -> lift (resume (provide p1 a)) >>= \case
    Done r -> return r
    Produced (b :: b) p1' -> lift (resume (provide p2 b)) >>= \case
      Done r -> return r
      Produced (c :: c) p2' -> yield c >>= provide (fuse p1' p2')

instance (Monad m) => Category (Consuming r m) where
  id = arr id
  (.) = flip fuse

instance (Monad m) => Arrow (Consuming r m) where
  arr f = Consuming $ foreverK (yield . f)
  -- Consuming r m b c -> Consuming r m (b, d) (c, d)
  first p = Consuming $ \(b, d) -> lift (resume (provide p b)) >>= \case
    Done r -> return r
    Produced c p' -> yield (c, d) >>= provide (first p')

instance (Monad m) => ArrowChoice (Consuming r m) where
  -- Consuming r m b c -> Consuming r m (Either b d) (Either c d)
  left p = Consuming go where
    go = \case
      Right d -> yield (Right d) >>= go
      Left b -> lift (resume (provide p b)) >>= \case
        Done r -> return r
        Produced c p' -> yield (Left c) >>= provide (left p')

