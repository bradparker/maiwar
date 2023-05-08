{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream.System.Timeout where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (lift)
import Data.Fixed (Micro, Pico, resolution)
import Data.Time (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Maiwar.Stream (Stream)
import qualified Maiwar.Stream as Stream
import qualified UnliftIO.Timeout

timeout :: forall o m a. MonadUnliftIO m => Int -> Stream o m a -> Stream o m (Maybe a)
timeout limit stream = do
  start <- liftIO getCurrentTime
  flip Stream.fold stream \action -> do
    now <- liftIO getCurrentTime
    let elapsedSeconds = nominalDiffTimeToSeconds (diffUTCTime now start)
    let microSecondResolution = fromInteger @Double (resolution (undefined :: Micro))
    let elapsedMicroseconds = floor (realToFrac @Pico @Double elapsedSeconds * microSecondResolution)
    let remaining = max 0 (limit - elapsedMicroseconds)
    step <- lift (UnliftIO.Timeout.timeout remaining action)
    case step of
      Nothing -> pure Nothing
      Just (Left a) -> pure (Just a)
      Just (Right (o, rest)) -> Stream.yield o *> rest
