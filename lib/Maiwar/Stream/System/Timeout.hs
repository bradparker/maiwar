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

-- This actually doesn't really work. It works if the only thing done
-- with the stream returned by this function is to run it. Otherwise
-- when other effectful operations are interleaved, say via `traverse`,
-- then if the '<<timeout>>' exception is thrown during those effects
-- it's not caught.
--
-- It might be possible to avoid this issue if interleaved effects are
-- appropriately masked. But that seems awfully fragile.
--
-- I wonder if there's a way to achieve this using MVars and timing
-- each layer in the stream. Might be slow and complex.
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
