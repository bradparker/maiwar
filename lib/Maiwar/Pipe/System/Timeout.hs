{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Pipe.System.Timeout where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Maiwar.Pipe (Pipe, pipe, runPipe)
import qualified Maiwar.Stream.System.Timeout as Stream

newtype TimeoutError = TimeoutError String
  deriving (Show)

instance Exception TimeoutError

timeoutError :: TimeoutError
timeoutError = TimeoutError "Timeout"

timeout ::
  forall i o m r.
  ( MonadIO m,
    MonadThrow m
  ) =>
  Int ->
  Pipe i o m r ->
  Pipe i o m r
timeout t p = pipe \stream -> do
  mr <- Stream.timeout t (runPipe p stream)
  case mr of
    Nothing -> throwM timeoutError
    Just (r, rest) -> pure (r, rest)
