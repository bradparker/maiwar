{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream.System.Timeout where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (lift)
import GHC.Clock (getMonotonicTimeNSec)
import Maiwar.Stream (Stream)
import qualified Maiwar.Stream as Stream
import qualified UnliftIO.Timeout

timeout :: forall o m a. (MonadUnliftIO m) => Int -> Stream o m a -> Stream o m (Maybe a)
timeout limit stream = do
  start <- liftIO getMonotonicTimeNSec
  Stream.for stream \action -> do
    now <- liftIO getMonotonicTimeNSec
    let elapsedNanoSeconds = now - start
    let elapsedMicroseconds = fromIntegral (elapsedNanoSeconds `div` 1000000)
    let remaining = max 0 (limit - elapsedMicroseconds)
    step <- lift (UnliftIO.Timeout.timeout remaining action)
    case step of
      Nothing -> pure Nothing
      Just (Left a) -> pure (Just a)
      Just (Right (o, rest)) -> Stream.yield o *> rest
