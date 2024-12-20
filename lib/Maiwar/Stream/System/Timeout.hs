{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream.System.Timeout where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, isEmptyMVar)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Event (getSystemTimerManager, registerTimeout)
import Maiwar.Stream (Stream, yield)
import qualified Maiwar.Stream as Stream

timeout :: forall o m a. (MonadIO m) => Int -> Stream o m a -> Stream o m (Maybe a)
timeout limit stream = do
  timedOut <- liftIO newEmptyMVar
  _ <- liftIO do
    tm <- getSystemTimerManager
    registerTimeout tm limit do
      putMVar timedOut ()
  Stream.for stream \action -> do
    step <- lift action
    case step of
      Left result -> pure (Just result)
      Right (a, rest) -> do
        continue <- liftIO (isEmptyMVar timedOut)
        if continue
          then do
            yield a
            rest
          else do
            pure Nothing
