{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Network.TCP.TLS where

import Control.Concurrent (ThreadId)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.ByteString (ByteString)
import Maiwar.Pipe (Pipe, evalPipe)
import Maiwar.Stream (Stream)
import qualified Maiwar.Stream as Stream
import Network.Simple.TCP (SockAddr, Socket)
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

acceptFork ::
  ServerParams ->
  Socket ->
  (SockAddr -> Pipe ByteString ByteString (ResourceT IO) ()) ->
  IO ThreadId
acceptFork params socket connectionHandler =
  TLS.acceptFork
    params
    socket
    \(context, addr) ->
      runResourceT
        . toContext context
        . evalPipe (connectionHandler addr)
        . fromContext
        $ context
