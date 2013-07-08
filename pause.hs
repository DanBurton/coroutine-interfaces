import Control.Monad (liftM)
import Control.Monad.Trans.Class

data Pause m
  = Run (m (Pause m))
  | Done

example1 :: Pause IO
example1 = Run $ do
  putStrLn "Step 1"
  return $ Run $ do
    putStrLn "Step 2"
    return $ Run $ do
      putStrLn "Step 3"
      return Done

fullRun :: Monad m => Pause m -> m ()
fullRun Done = return ()
fullRun (Run m) = m >>= fullRun

-- >>> fullRun example1
-- Step 1
-- Step 2
-- Step 3

runN :: Monad m => Int -> Pause m -> m (Pause m)
runN 0 p = return p
runN _ Done = return Done
runN n (Run m)
  | n < 0     = fail "Invalid argument to runN"
  | otherwise = m >>= runN (n - 1)

-- >>> rest <- runN 2 example1
-- Step 1
-- Step 2
-- >>> () <- fullRun rest
-- Step 3
-- >>> Done <- runN 5 rest
-- Step 3


data PauseT m r
  = RunT (m (PauseT m r))
  | DoneT r

instance (Show r) => Show (PauseT m r) where
  show (RunT _) = "Run{ ... }"
  show (DoneT r) = "Done{ " ++ show r ++ " }"

instance (Monad m) => Monad (PauseT m) where
  return a = DoneT a
  DoneT r >>= f = f r
  RunT m >>= f = RunT $ liftM (>>= f) m

instance MonadTrans PauseT where
  lift m = RunT $ liftM DoneT m

pause :: Monad m => PauseT m ()
pause = DoneT ()

example2 :: PauseT IO ()
example2 = do
  lift $ putStrLn "Step 1"
  pause
  lift $ putStrLn "Step 2"
  pause
  lift $ putStrLn "Step 3"

fullRunT :: PauseT IO r -> IO r
fullRunT (DoneT r) = return r
fullRunT (RunT m) = m >>= fullRunT

example3 :: PauseT IO ()
example3 = do
  _ <- return "hello"
  _ <- return "world"
  _ <- return "beep"
  return ()

runNT :: Int -> PauseT IO r -> IO (PauseT IO r)
runNT 0 p = return p
runNT _ d@DoneT{} = return d
runNT n (RunT m) = m >>= runNT (n - 1)

