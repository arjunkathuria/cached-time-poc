{-# LANGUAGE FlexibleContexts #-}

module MyLib where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
import Control.Monad.Reader (MonadIO, MonadReader, ask, asks, forever, liftIO)
import Data.Time (getCurrentTime)
import Types

makeAppState :: TVar CachedTime -> AppState
makeAppState timeTvar = AppState {cachedTime = timeTvar}

getCachedTime :: (MonadReader AppState m, MonadIO m) => m CachedTime
getCachedTime = do
  -- appstate <- ask
  cachedTimeRef <- asks cachedTime
  cachedTimeValue <- liftIO $ readTVarIO $ cachedTimeRef
  pure cachedTimeValue

updateCachedTimeToCurrent :: (MonadReader AppState m, MonadIO m) => m ()
updateCachedTimeToCurrent = do
  appstate <- ask
  currentTime <- liftIO $ getCurrentTime -- time library fn
  liftIO $ atomically $ writeTVar (cachedTime appstate) currentTime

oneMillisec :: MicroSeconds
oneMillisec = 1000 -- 1ms == 1000us

threadDelayUpdateCachedTime :: (MonadReader AppState m, MonadIO m) => MilliSeconds -> m ()
threadDelayUpdateCachedTime delay = do
  liftIO $ threadDelay delayTime -- wait for sometime
  currentCachedTime <- getCachedTime
  liftIO $ putStrLn $ "Current cached Time: " <> show currentCachedTime
  updateCachedTimeToCurrent
  updatedTime <- getCachedTime
  liftIO $ putStrLn $ "Updated cached Time: " <> show updatedTime <> "\n"
  where
    delayTime :: MicroSeconds
    delayTime = oneMillisec * delay

loopUpdateCachedTimeForever :: (MonadReader AppState m, MonadIO m) => Int -> m ()
loopUpdateCachedTimeForever delay = forever $ threadDelayUpdateCachedTime delay
