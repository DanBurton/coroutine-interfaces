module Control.Coroutine.Interface.Prelude (
  printer,
  fromList,
  ) where

import Control.Coroutine.Interface
import Control.Util
import Control.Monad
import Control.Monad.Trans.Class (lift)
import qualified Control.Proxy.Core.Fast as P
import qualified Control.Proxy as P

await = yield ()

printer :: Show a => Consuming r IO a ()
printer = Consuming $ foreverK $ (\a -> lift (print a) >> await)

fromList :: Monad m => [a] -> Producing a () m ()
fromList = mapM_ yield
