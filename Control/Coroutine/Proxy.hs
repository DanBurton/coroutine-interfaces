{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}

module Control.Coroutine.Proxy (
  ) where

import Control.Category
import Control.Arrow
import Prelude hiding (id, (.))
import Control.Monad
import Control.Coroutine.Interface
import Control.Monad.Trans.Class (lift)
import Data.Function (fix)
import Control.Util
import qualified Control.IxM as IxM
import Data.Void (Void, absurd)

newtype Proxy r m downI upI
  = Proxy { unProxy :: Consuming r (Producing (Fst upI) (Snd upI) m) (Fst downI) (Snd downI) }

type family Fst (xy :: (*,*)) :: *
type family Snd (xy :: (*,*)) :: *
type instance Fst '(x,y) = x
type instance Snd '(x,y) = y

idProxy :: Monad m => Proxy r m '(a,b) '(a,b)
idProxy = Proxy $ Consuming $ fix ((lift . yield >=> yield) >=>)


commute :: Monad m =>
  Producing a b (Producing c d m) r -> Producing c d (Producing a b m) r
commute p = insert2 p $$ Consuming (insert1 . provide (unProxy idProxy))

infixl 1 $=
infixr 2 =$
infixr 2 =$=


($=) :: Monad m => Producing a b m r -> Proxy r m '(a,b) '(c,d) -> Producing c d m r
producing $= Proxy proxy = insert1 producing $$ proxy

(=$) :: Monad m => Proxy r m '(a,b) '(c,d) -> Consuming r m c d -> Consuming r m a b
Proxy proxy =$ consuming = Consuming $ \a' ->
  commute (provide proxy a') $$ Consuming (insert1 . provide consuming)

(=$=) :: Monad m =>
  Proxy r m '(a,a') '(b,b') -> Proxy r m '(b,b') '(c,c') -> Proxy r m '(a,a') '(c,c')
Proxy proxyl =$= Proxy proxyr = Proxy $ Consuming $ \a ->
  insert2 (commute (provide proxyl a)) $$ Consuming (insert1 . provide proxyr)

promote :: Monad m => Consuming r m a b -> Proxy r m '(c,b) '(c,a)
promote k = Proxy $ Consuming $ \c -> do
  a <- lift (yield c)
  IxM.duplicate (provide k a)

voidConsumer :: Monad m => Consuming r m a ()
voidConsumer = void id

absurdConsumer :: Monad m => Consuming r m Void v
absurdConsumer = fmap absurd id

source :: Monad m => Producing b () m r -> Proxy r m '(c,b) '(c,a)
source p = promote ((Consuming $ \() -> p) . voidConsumer)

--sink :: Monad m => Producing () a m r -> Proxy r m '(c'
sink p = promote (absurdConsumer . p)

unitSpewer :: Monad m => Producing () a m r
unitSpewer = provide voidConsumer undefined

-- runProxy :: Monad m => Proxy r m '((),()) '((),()) -> m r
runProxy p = unitSpewer $= p $$ id
