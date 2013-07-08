import Control.Monad.Trans.Cont (ContT(ContT, runContT))
import Data.Monoid (Monoid, mempty, mappend)
import Control.Monad (liftM2)

instance (Monoid r, Monad m) => Monoid (ContT r m a) where
  mempty = ContT $ const $ return mempty
  m `mappend` n = ContT $ \ c -> liftM2 mappend (runContT m c) (runContT n c)

