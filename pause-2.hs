import Control.Coroutine.Interface
import Control.Monad.Trans.Class (lift)
import Control.Monad

type PauseT = Producing () ()
pause = yield ()

example2 :: PauseT IO ()
example2 = do
  lift $ putStrLn "Step 1"
  lift $ putStrLn "Step 1"
  pause
  lift $ putStrLn "Step 2"

runN :: Monad m => Int -> PauseT m r -> m (PauseT m r)
runN 0 p = return p
runN i p
  | i < 0 = error "Invalid argument to runN"
  | otherwise =
    resume p >>= \s -> case s of
      Done r -> return (return r)
      Produced () k -> runN (i-1) (provide k ())
