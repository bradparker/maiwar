{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream.System.Timeout where

import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay, throwTo)
import Control.Exception (Exception (..), SomeAsyncException, asyncExceptionFromException, asyncExceptionToException)
import Control.Exception.Safe (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Unique (Unique, newUnique)
import Maiwar.Stream (Stream)
import Maiwar.Stream.Exception as Stream

newtype Timeout = Timeout Unique deriving (Eq)

instance Show Timeout where
  show _ = "<<timeout>>"

instance Exception Timeout where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

timeout :: forall o m a. (MonadIO m, MonadCatch m) => Int -> Stream o m a -> Stream o m (Maybe a)
timeout limit stream = do
  pid <- liftIO myThreadId
  ex <- liftIO (Timeout <$> newUnique)
  threadId <-
    liftIO
      ( forkIO do
          threadDelay limit
          throwTo pid ex
      )
  result <- Stream.tryAsync @SomeAsyncException stream
  liftIO (killThread threadId)
  case result of
    Left _ -> pure Nothing
    Right a -> pure (Just a)
