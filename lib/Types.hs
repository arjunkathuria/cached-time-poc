{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import Control.Monad.Reader (ReaderT)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

type CachedTime = UTCTime

data AppState = AppState
  { cachedTime :: !(TVar CachedTime)
  }
  deriving (Generic)

type MilliSeconds = Int

type MicroSeconds = Int

type App = ReaderT AppState IO

instance (NFData a) => NFData (TVar a) where
  rnf x = x `seq` ()

instance NFData AppState

data MyException = ThisException | ThatException
  deriving (Show)

instance Exception MyException
