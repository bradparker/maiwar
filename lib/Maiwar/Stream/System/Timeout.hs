{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream.System.Timeout where

import Control.Concurrent
  ( forkIOWithUnmask,
    killThread,
    newEmptyMVar,
    putMVar,
    threadDelay,
    tryTakeMVar,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (isJust)
import Maiwar.Stream (Stream)
import qualified Maiwar.Stream as Stream

timeout ::
  forall a m r.
  ( MonadIO m
  ) =>
  Int ->
  Stream a m r ->
  Stream a m (Maybe r)
timeout t stream
  | t < 0 = Just <$> stream
  | t == 0 = pure Nothing
  | otherwise = do
      done <- liftIO newEmptyMVar
      timerThreadId <- liftIO do
        forkIOWithUnmask \unmask -> do
          unmask (threadDelay t >> putMVar done ())
      r <- Stream.unfold
        ( \s -> do
            shouldStop <- isJust <$> liftIO (tryTakeMVar done)
            if shouldStop
               then pure (Left Nothing)
               else Stream.next s
        )
        (Just <$> stream)
      liftIO (killThread timerThreadId)
      pure r
