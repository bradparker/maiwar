{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Pipe.System.Timeout where

import Control.Applicative (Alternative, empty)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (lift)
import Maiwar.Pipe (Pipe)
import qualified Maiwar.Pipe as Pipe
import qualified Maiwar.Stream.System.Timeout as Stream

timeout ::
  forall i o m a.
  (MonadUnliftIO m, Alternative m) =>
  Int ->
  Pipe i o m a ->
  Pipe i o m a
timeout limit pipe = Pipe.pipe \s -> do
  result <- Stream.timeout limit (Pipe.runPipe pipe s)
  case result of
    Nothing -> lift empty
    Just a -> pure a
