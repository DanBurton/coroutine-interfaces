{-# LANGUAGE ScopedTypeVariables #-} -- this comes in handy
{-# LANGUAGE EmptyDataDecls, KindSignatures #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Morph (MFunctor, hoist)

data Producing o i (m :: * -> *) r -- don't rely on the internals of this
instance (Monad m) => Monad (Producing o i m) where
instance MonadTrans (Producing o i) where
instance MFunctor (Producing o i) where

newtype Consuming r m i o
  = Consuming { provide :: i -> Producing o i m r }

newtype Proxy r m upI downI
  = Proxy { unProxy :: Consuming r (Producing (Fst downI) (Snd downI) m) (Fst upI) (Snd upI) }

type family Fst (xy :: (*,*)) :: *
type family Snd (xy :: (*,*)) :: *
type instance Fst '(x,y) = x
type instance Snd '(x,y) = y

infixl 0 $$
($$) :: Monad m => Producing a b m r -> Consuming r m a b -> m r
producing $$ consuming = undefined -- take this as a given

commute :: Monad m => Producing a b (Producing c d m) r
                   -> Producing c d (Producing a b m) r
commute = undefined -- take this as a given


insert0 :: Monad m =>
  m r -> Producing a b m r
insert0 = lift

insert1 :: (MFunctor t, Monad m) =>
  t m r -> t (Producing a b m) r
insert1 = hoist insert0

insert2 :: (MFunctor t, MFunctor t2, Monad m, Monad (t m)) =>
  t2 (t m) r -> t2 (t (Producing a b m)) r
insert2 = hoist insert1


(=$) :: forall a b c d m r. Monad m =>
  Proxy r m '(a,b) '(c,d) -> Consuming r m c d -> Consuming r m a b
Proxy proxy =$ consuming = Consuming $ \(a :: a) ->
  let
    p :: Producing c d (Producing b a m) r
    p = commute (provide proxy a)
    c :: Consuming r (Producing b a m) c d
    c = Consuming (insert1 . provide consuming)
  in
    p $$ c

(=$=) :: forall a a' b b' c c' m r. Monad m =>
  Proxy r m '(a,a') '(b,b') -> Proxy r m '(b,b') '(c,c') -> Proxy r m '(a,a') '(c,c')
Proxy proxyl =$= Proxy proxyr = Proxy $ Consuming $ \(a :: a) ->
  let
    p :: Producing b b' (Producing a' a (Producing c c' m)) r
    p = insert2 (commute (provide proxyl a))
    c :: Consuming r (Producing a' a (Producing c c' m)) b b'
    c = Consuming $ insert1 . provide proxyr
  in
    p $$ c
