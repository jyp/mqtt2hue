module Semaphores where


import Data.Map
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

type SMap k = Map k (MVar ())

type MultiSem k = MVar (SMap k)

waitForSemaphore :: Ord k => k -> MultiSem k -> IO ()
waitForSemaphore k s = do
  v <- newEmptyMVar
  modifyMVar_ s (pure . insert k v)
  takeMVar v

waitForSemaphoreAtMost :: Ord k => Int -> k -> MultiSem k -> IO ()
waitForSemaphoreAtMost d k s = do
  v <- newEmptyMVar
  modifyMVar_ s (pure . insert k v)
  _ <- forkIO $ do
    threadDelay d
    _ <- tryPutMVar v ()
    return ()
  takeMVar v

waitForSemaphoresAtMost :: (Ord k, Show k) => Int -> [k] -> MultiSem k -> IO ()
waitForSemaphoresAtMost d ks s = do
  vs <- forM ks $ \k -> do
    v <- newEmptyMVar
    modifyMVar_ s (pure . insert k v)
    return v
  _ <- forkIO $ do
    threadDelay d
    forM_ vs $ \ v -> tryPutMVar v ()
  forM_ vs $ \ v -> takeMVar v 


signalSemaphore :: Ord k => k -> MultiSem k -> IO ()
signalSemaphore k s = do
  withMVar s $ \m -> case Data.Map.lookup k m of
    Nothing -> return ()
    Just v -> do
      _ <- tryPutMVar v ()
      return ()
