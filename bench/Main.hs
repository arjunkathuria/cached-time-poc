module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq (NFData (..), deepseq, rnf, rwhnf)
import Control.Monad.Reader (runReaderT)
import Criterion
import Criterion.Main (defaultMain)
import Data.Time (getCurrentTime)
import MyLib
import Types

-- NFData instance for Async a was missing
instance (NFData a) => NFData (Async a) where
  rnf = rwhnf -- evaluate weakly for now

setupEnv :: IO (AppState, Async ())
setupEnv = do
  currentTime <- getCurrentTime
  currentTimeRef <- newTVarIO currentTime
  let appstate = makeAppState currentTimeRef
  timerThread <- async $ runReaderT (loopUpdateCachedTimeForever timerThreadUpdateInterval) appstate
  -- timerThread <- async $ runReaderT (threadDelayUpdateCachedTime timerThreadUpdateInterval) appstate
  link timerThread
  pure (appstate, timerThread)
  where
    timerThreadUpdateInterval :: MilliSeconds
    timerThreadUpdateInterval = 10 -- update every 10ms

benchmain :: IO ()
benchmain =
  defaultMain
    [ bench "getCurrentTime from IO" $ nfIO getCurrentTime,
      bgroup
        "Cached time"
        [ envWithCleanup
            setupEnv -- setup environment
            (\ ~(_, timerThread) -> cancel timerThread) -- cleanup environment
            ( \ ~(appState, _) ->
                -- benchmark to run
                bench "Get Cached Time from AppState" $ nfIO $ do
                  currTime <- runReaderT getCachedTime appState
                  deepseq currTime (pure ()) -- try and make sure the value is evaluated
                  -- putStrLn $ show currTime
                  pure currTime
            ),
          envWithCleanup
            setupEnv -- setup environment
            (\ ~(_, timerThread) -> cancel timerThread) -- cleanup environment
            ( \ ~(appState, _) ->
                bench
                  "Read cachedTime Directly"
                  $ nfIO (readTVarIO $ cachedTime appState) -- benchmark to run
            )
        ]
    ]

main :: IO ()
main = benchmain
