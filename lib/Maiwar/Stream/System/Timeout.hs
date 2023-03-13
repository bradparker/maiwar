{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream.System.Timeout where

import Control.Concurrent
  ( forkIOWithUnmask,
    killThread,
    myThreadId,
    threadDelay,
    throwTo,
  )
import Control.Exception
  ( Exception (..),
    asyncExceptionFromException,
    asyncExceptionToException,
    mask_,
    uninterruptibleMask_,
  )
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, onException, throwM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Unique (Unique, newUnique)
import Maiwar.Stream (Stream)

newtype Timeout = Timeout Unique deriving (Eq)

instance Show Timeout where
  show _ = "<<timeout>>"

instance Exception Timeout where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

timeout ::
  forall a m r.
  ( MonadIO m,
    MonadCatch m,
    MonadThrow m
  ) =>
  Int ->
  Stream a m r ->
  Stream a m (Maybe r)
timeout t stream
  | t < 0 = Just <$> stream
  | t == 0 = Nothing <$ stream
  | otherwise = do
      (ex, timerThreadId) <- liftIO $ mask_ do
        pid <- myThreadId
        ex <- Timeout <$> newUnique
        timerThreadId <- forkIOWithUnmask \unmask -> do
          unmask (threadDelay t >> throwTo pid ex)
        pure (ex, timerThreadId)
      let after = liftIO do
            uninterruptibleMask_ (killThread timerThreadId)
      let action = do
            result <- (Just <$> stream) `onException` after
            after
            pure result
      action `catch` \e -> do
        if e == ex
          then pure Nothing
          else throwM e