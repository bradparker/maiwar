{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Pipe.System.Timeout where

import Control.Monad.IO.Class (MonadIO)
import Maiwar.Pipe (Pipe)
import qualified Maiwar.Stream.System.Timeout as Stream
import qualified Maiwar.Pipe as Pipe

timeout ::
  forall i o m a.
  (MonadIO m) =>
  Int ->
  Pipe i o m a ->
  Pipe i o m (Maybe a)
timeout limit pipe = do
  Pipe.pipe \s -> do
    result <- Stream.timeout limit (Pipe.runPipe pipe (Stream.timeout limit s))
    case result of
      -- TODO: Pipes need some concept of bailing built in I think. Some way for a consumer to say "I can't go on, and neither can anyone else"
      Nothing -> pure (Nothing, undefined)
      -- TODO: Need to reset the rest stream here so that it doesn't time out for subsequent pipes.
      Just (a, rest) -> pure (Just a, rest >>= maybe undefined pure)
