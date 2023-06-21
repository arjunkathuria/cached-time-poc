module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader (runReaderT)
import Data.Time
import MyLib
import Types

-- Poll an Async Thread `tid` every `delay` time interval
-- take some `action` based on the polling result.
pollThread :: Async a -> MilliSeconds -> IO a -> IO ()
pollThread tid delay action = go tid
  where
    go threadid = do
      pollResult <- poll threadid
      putStr "Polling:- "
      case pollResult of
        Nothing -> do
          -- Still Working, need to do nothing
          putStr "Timer Thread Still Working\n"
          threadDelay (delay * oneMillisec)
          go threadid
        Just _ -> do
          -- thread completed or died for some reason, respawn.
          respawnedThread <- async action
          -- link respawnedThread
          putStr "Timer Thread Completed/Died. Respawned!\n"
          go respawnedThread

main :: IO ()
main = do
  currentTime <- getCurrentTime
  currentTimeRef <- newTVarIO currentTime
  let appstate = makeAppState currentTimeRef

  -- This thread goes on forever
  timerThread <- async $ runReaderT (loopUpdateCachedTimeForever timerThreadUpdateInterval) appstate

  -- This thread completes after a time-delay provided, simulating the timer thread dying/completing.
  -- timerThread <- async $ runReaderT (threadDelayUpdateCachedTime timerThreadUpdateInterval) appstate

  link timerThread

  pollThread timerThread pollingThreadPollInterval (runReaderT (loopUpdateCachedTimeForever timerThreadUpdateInterval) appstate)
  -- pollThread timerThread pollingThreadPollInterval (runReaderT (threadDelayUpdateCachedTime timerThreadUpdateInterval) appstate)

  putStrLn $ "Program has Ended!" <> show currentTime
  where
    timerThreadUpdateInterval :: MilliSeconds
    timerThreadUpdateInterval = 2000 -- update every 2 seconds
    pollingThreadPollInterval :: MilliSeconds
    pollingThreadPollInterval = 1000 -- poll every 1 second
