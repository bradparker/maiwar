{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Network.TCP where

import Control.Exception.Safe (bracket, bracket_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed.Safe (Managed, runManaged)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Foreign.C (CInt)
import Maiwar.Pipe (Pipe, evalPipe)
import Maiwar.Stream (Stream)
import qualified Maiwar.Stream as Stream
import Network.Simple.TCP (HostPreference, ServiceName, Socket, recv, send)
import qualified Network.Simple.TCP as TCP
import qualified Network.Socket as Socket
import qualified System.Posix.Internals as System.Posix

toSocket :: forall m r. MonadIO m => Socket -> Stream ByteString m r -> m r
toSocket socket = Stream.run . Stream.traverse (send socket)

fromSocket :: forall m. MonadIO m => Int -> Socket -> Stream ByteString m ()
fromSocket size = Stream.unfold \socket -> do
  input <- liftIO (recv socket size)
  pure case input of
    Nothing -> Left ()
    Just bytes -> Right (bytes, socket)

accept :: IO () -> IO () -> Socket -> Pipe ByteString ByteString Managed () -> IO ()
accept before after socket connectionHandler =
  void
    ( TCP.acceptFork
        socket
        \(csocket, _) ->
          bracket_ before after
            . runManaged
            . toSocket csocket
            . evalPipe connectionHandler
            . fromSocket 16384
            $ csocket
    )

data OwnSocketConfig = OwnSocketConfig
  { hostPreference :: HostPreference,
    port :: ServiceName
  }
  deriving (Show)

newtype ProvidedSocketConfig = ProvidedSocketConfig
  { fd :: CInt
  }
  deriving (Show)

data ListeningConfig
  = OwnSocket OwnSocketConfig
  | ProvidedSocket ProvidedSocketConfig
  deriving (Show)

listen :: (MonadIO m, MonadMask m) => ListeningConfig -> (Socket -> m ()) -> m ()
listen config action =
  case config of
    OwnSocket ownSocketConfig ->
      TCP.listen
        ownSocketConfig.hostPreference
        ownSocketConfig.port
        (action . fst)
    ProvidedSocket providedSocketConfig ->
      bracket
        ( do
            socket <- liftIO (Socket.mkSocket providedSocketConfig.fd)
            liftIO do
              Socket.setSocketOption socket Socket.NoDelay 1
              Socket.setSocketOption socket Socket.ReuseAddr 1
              Socket.setSocketOption socket Socket.KeepAlive 1
              Socket.withFdSocket socket (`System.Posix.setNonBlockingFD` True)
            TCP.listenSock socket 2048
            pure socket
        )
        TCP.closeSock
        action
