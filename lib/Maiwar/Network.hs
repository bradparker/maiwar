{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Network where

import Control.Concurrent (forkIO)
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
import Control.Exception.Safe (catch)
import Control.Monad (when)
import Control.Monad.Managed.Extra (Managed, around)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Maiwar.Network.HTTP (Handler, handleConnection)
import Maiwar.Network.TCP (ListeningConfig)
import qualified Maiwar.Network.TCP as TCP
import qualified Maiwar.Network.TCP.TLS as TLS
import Network.Simple.TCP (Socket)
import Network.Simple.TCP.TLS (ServerParams)
import qualified System.IO
import qualified System.Posix.Signals

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

serve :: Config -> Handler ByteString ByteString Managed () -> IO ()
serve config handler = do
  shutdown <- newEmptyTMVarIO @()
  connectionCount <- newTVarIO @Int 0
  installShutdownHandler shutdown
  let accept = maybe TCP.accept TLS.accept config.tls
      shouldAccept = atomically (isEmptyTMVar shutdown)
      onConnection = atomically (modifyTVar connectionCount (+ 1))
      afterConnection = atomically (modifyTVar connectionCount (subtract 1))
  listen \socket -> do
    whileM_
      shouldAccept
      ( catch
          ( accept socket do
              around onConnection afterConnection
              handleConnection handler
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

    listen :: (Socket -> IO ()) -> IO ()
    listen = void . forkIO . TCP.listen config.listen

    handleAcceptException :: SomeException -> IO ()
    handleAcceptException = System.IO.hPrint System.IO.stderr
