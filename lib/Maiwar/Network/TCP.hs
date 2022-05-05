{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Network.TCP where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Maiwar.Stream (Stream)
import qualified Maiwar.Stream as Stream
import Network.Simple.TCP (Socket, recv, send)

toSocket :: forall m r. MonadIO m => Socket -> Stream ByteString m r -> m r
toSocket socket = Stream.run . Stream.traverse (send socket)

fromSocket :: forall m. MonadIO m => Int -> Socket -> Stream ByteString m ()
fromSocket size = Stream.unfold \socket -> do
  input <- liftIO (recv socket size)
  pure case input of
    Nothing -> Left ()
    Just bytes -> Right (bytes, socket)
