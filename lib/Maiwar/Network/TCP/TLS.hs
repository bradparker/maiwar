{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Network.TCP.TLS where

import Control.Exception.Safe (bracket_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed.Safe (Managed, runManaged)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Maiwar.Pipe (Pipe, evalPipe)
import Maiwar.Stream (Stream)
import qualified Maiwar.Stream as Stream
import Network.Simple.TCP (Socket)
import Network.Simple.TCP.TLS (Context, ServerParams, recv, send)
import qualified Network.Simple.TCP.TLS as TLS

toContext :: forall m r. MonadIO m => Context -> Stream ByteString m r -> m r
toContext context = Stream.run . Stream.traverse (send context)

fromContext :: forall m. MonadIO m => Context -> Stream ByteString m ()
fromContext = Stream.unfold \context -> do
  input <- liftIO (recv context)
  pure case input of
    Nothing -> Left ()
    Just bytes -> Right (bytes, context)

accept ::
  ServerParams ->
  IO () ->
  IO () ->
  Socket ->
  Pipe ByteString ByteString Managed () ->
  IO ()
accept params before after socket connectionHandler =
  void
    ( TLS.acceptFork
        params
        socket
        \(context, _) ->
          bracket_ before after
            . runManaged
            . toContext context
            . evalPipe connectionHandler
            . fromContext
            $ context
    )
