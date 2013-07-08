{-# LANGUAGE DataKinds #-}

import qualified Control.Proxy as P
import qualified Control.Proxy.Core.Fast as PF
import Control.Coroutine.Interface
import Control.Coroutine.Proxy
import qualified Control.MTrans as MT
import Control.Monad.Trans.Class (MonadTrans, lift)
import Prelude hiding (id, (.))
import Control.Category

toProxy' :: (P.Proxy p, MonadTrans (p ou iu id od), Monad (p ou iu id od m), Monad m) =>
  Producing od id (Producing ou iu m) r -> p ou iu id od m r
toProxy' = pfold id return P.respond . MT.map (pfold lift return P.request)

toProxy :: (P.Proxy p, MonadTrans (p ou iu id od), Monad (p ou iu id od m), Monad m) =>
  Proxy r m '(id,od) '(ou,iu) -> id -> p ou iu id od m r
toProxy (Proxy k) a = toProxy' (provide k a)

fromProxy' :: Monad m => P.ProxyFast ou iu id od m r -> Producing od id (Producing ou iu m) r
fromProxy' (PF.Request o k) = request o >>= fromProxy' . k
fromProxy' (PF.Respond o k) = respond o >>= fromProxy' . k
fromProxy' (PF.M m) = lift (lift m) >>= fromProxy'
fromProxy' (PF.Pure r) = return r

fromProxy :: Monad m => (id -> P.ProxyFast ou iu id od m r) -> Proxy r m '(id,od) '(ou,iu)
fromProxy k = Proxy $ Consuming $ \a -> fromProxy' (k a)
