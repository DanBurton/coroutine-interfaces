{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}

module Control.Coroutine.Proxy where

import Control.Category
import Control.Arrow
import Prelude hiding (id, (.))
import Control.Monad
import Control.Coroutine.Interface
import Control.Monad.Trans.Class (lift)
import Data.Function (fix)
import Control.Util
import qualified Control.IxM as IxM
import Control.Applicative

newtype Proxy r m downI upI
  = Proxy { unProxy :: Consuming r (Producing (Fst upI) (Snd upI) m) (Fst downI) (Snd downI) }

newtype Pipe r m a b
  = Pipe { unPipe :: Proxy r m '((),b) '((),a) }

type PipeM a b m r = Producing b () (Producing () a m) r

instance (Monad m) => Category (Pipe r m) where
  id = Pipe idProxy
  Pipe p1 . Pipe p2 = Pipe (p1 =$= p2)

instance (Monad m) => Functor (Pipe r m a) where
  fmap f (Pipe (Proxy k)) = Pipe (Proxy (fmap f k))

instance (Monad m) => Applicative (Pipe r m a) where
  pure x = Pipe (Proxy (pure x))
  Pipe (Proxy kf) <*> Pipe (Proxy kx) = Pipe (Proxy (kf <*> kx))

request :: Monad m => o2 -> Producing o1 i1 (Producing o2 i2 m) i2
request = lift . yield

respond :: Monad m => o1 -> Producing o1 i1 (Producing o2 i2 m) i1
respond = yield

await :: Monad m => PipeM a b m a
await = request ()

proxyC :: (i1 -> Producing o1 i1 (Producing o2 i2 m) r) -> Proxy r m '(i1,o1) '(o2,i2)
proxyC = Proxy . Consuming

pipe :: PipeM a b m r -> Pipe r m a b
pipe p = Pipe $ proxyC $ const p

type family Fst (xy :: (*,*)) :: *
type family Snd (xy :: (*,*)) :: *
type instance Fst '(x,y) = x
type instance Snd '(x,y) = y

idProxy :: Monad m => Proxy r m '(a,b) '(a,b)
idProxy = proxyC $ foreverK (request >=> respond)

-- instance (Monad m) => Category (Proxy r m) where
--   id = idProxy
--   (.) = flip (=$=)


commute :: Monad m =>
  Producing a b (Producing c d m) r -> Producing c d (Producing a b m) r
commute p = insert2 p $$ overC insert1 (unProxy idProxy)


newtype YieldC m outer inter
  = YieldC { unYieldC :: Snd outer -> Producing (Snd inter) (Fst inter) m (Fst outer) }

(/>/) :: Monad m =>
  YieldC m '(b,b') '(c,c') -> YieldC m '(a,a') '(b,b') -> YieldC m '(a,a') '(c,c')
YieldC p1 />/ YieldC p2 = YieldC (\a' -> replaceYield p1 (p2 a'))

-- instance (Monad m) => Category (YieldC m) where
--   id = YieldC yield
--   (.) = (/>/)


infixl 1 $=
infixr 2 =$
infixr 2 =$=


($=) :: Monad m => Producing a b m r -> Proxy r m '(a,b) '(c,d) -> Producing c d m r
producing $= Proxy p = insert1 producing $$ p

(=$) :: Monad m => Proxy r m '(a,b) '(c,d) -> Consuming r m c d -> Consuming r m a b
Proxy p =$ consuming = Consuming $ \a' ->
  commute (provide p a') $$ overC insert1 consuming

(=$=) :: Monad m =>
  Proxy r m '(a,a') '(b,b') -> Proxy r m '(b,b') '(c,c') -> Proxy r m '(a,a') '(c,c')
Proxy pl =$= Proxy pr = Proxy $ Consuming $ \a ->
  insert2 (commute (provide pl a)) $$ overC insert1 pr

promote :: Monad m => Consuming r m a b -> Proxy r m '(c,b) '(c,a)
promote k = Proxy $ Consuming $ \c -> do
  a <- request c
  IxM.duplicate (provide k a)

voidConsumer :: Monad m => Consuming r m a ()
voidConsumer = void id
-- voidConsumer = push unitSpewer

source :: Monad m => Producing b () m r -> Proxy r m '(c,b) '(c,a)
source p = promote (Consuming (const p) . voidConsumer)

--sink :: Monad m => Producing () a m r -> Proxy r m '(c'
sink p = promote (voidConsumer . p)

unitSpewer :: Monad m => Producing () a m r
unitSpewer = forever $ yield ()

-- runProxy :: Monad m => Proxy r m '((),()) '((),()) -> m r
runProxy p = unitSpewer $= p $$ id

runPipe :: Conduit () () -> IO ()
runPipe (Pipe p) = runProxy p

type Conduit = Pipe () IO
type Consumer a = Conduit a ()
type Producer b = Conduit () b

fromList :: [a] -> Producer a
fromList = pipe . mapM_ yield

takeP :: Int -> Conduit a a
takeP n = pipe $ replicateM_ n (await >>= yield)

printer :: Show b => Consumer b
printer = pipe $ forever (await >>= lift . lift . print)
