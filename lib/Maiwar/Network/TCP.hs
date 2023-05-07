{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Network.TCP where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM
  ( TMVar,
    atomically,
    isEmptyTMVar,
    modifyTVar,
    newEmptyTMVarIO,
    newTVarIO,
    putTMVar,
    readTVar,
    retry,
    takeTMVar,
  )
import Control.Exception (SomeException)
import Control.Exception.Safe (bracket, catch)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Foreign.C (CInt)
import qualified Maiwar.Network.TCP.TLS as TLS
import Maiwar.Pipe (Pipe, evalPipe)
import Maiwar.Stream (Stream)
import qualified Maiwar.Stream as Stream
import Network.Simple.TCP
  ( HostPreference,
    ServiceName,
    SockAddr,
    Socket,
    recv,
    send,
  )
import qualified Network.Simple.TCP as TCP
import Network.Simple.TCP.TLS (ServerParams)
import qualified Network.Socket as Socket
import qualified System.IO
import qualified System.Posix.Internals as System.Posix
import qualified System.Posix.Signals

toSocket :: MonadIO m => Socket -> Stream ByteString m r -> m r
toSocket socket = Stream.run . Stream.traverse (send socket)

fromSocket :: MonadIO m => Int -> Socket -> Stream ByteString m ()
fromSocket size = Stream.unfold \socket -> do
  input <- liftIO (recv socket size)
  pure case input of
    Nothing -> Left ()
    Just bytes -> Right (bytes, socket)

acceptFork :: Socket -> (SockAddr -> Pipe ByteString ByteString (ResourceT IO) ()) -> IO ThreadId
acceptFork socket connectionHandler =
  TCP.acceptFork
    socket
    \(csocket, addr) ->
      runResourceT
        . toSocket csocket
        . evalPipe (connectionHandler addr)
        . fromSocket 16384
        $ csocket

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

data Config = Config
  { tls :: Maybe ServerParams,
    listen :: ListeningConfig
  }
  deriving (Show)

whileM_ :: Monad m => m Bool -> m () -> m ()
whileM_ predicate action = go
  where
    go = do
      x <- predicate
      when x do
        action
        go

serve :: Config -> (SockAddr -> Pipe ByteString ByteString (ResourceT IO) ()) -> IO ()
serve config handler = do
  shutdown <- newEmptyTMVarIO @()
  connectionCount <- newTVarIO @Int 0
  installShutdownHandler shutdown
  let accept = maybe acceptFork TLS.acceptFork config.tls
      shouldAccept = atomically (isEmptyTMVar shutdown)
      onConnection = atomically (modifyTVar connectionCount (+ 1))
      afterConnection = atomically (modifyTVar connectionCount (subtract 1))
  (void . forkIO . listen config.listen) \socket -> do
    whileM_
      shouldAccept
      ( catch
          ( void
              ( accept socket \addr -> do
                  liftIO onConnection
                  handler addr
                  liftIO afterConnection -- TODO: ensure this runs???
              )
          )
          handleAcceptException
      )
  atomically do
    takeTMVar shutdown
    activeConnections <- readTVar connectionCount
    when (activeConnections /= 0) retry
  where
    installShutdownHandler :: TMVar () -> IO ()
    installShutdownHandler shutdown =
      void
        ( System.Posix.Signals.installHandler
            System.Posix.Signals.sigTERM
            (System.Posix.Signals.CatchOnce (atomically (putTMVar shutdown ())))
            Nothing
        )

    handleAcceptException :: SomeException -> IO ()
    handleAcceptException = System.IO.hPrint System.IO.stderr
