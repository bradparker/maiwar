{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.CLI where

import Control.Applicative (optional, (<|>))
import Control.Monad (guard)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed (Managed)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
import Foreign.C (CInt)
import Maiwar
  ( Config (Config),
    Handler,
    OwnSocketConfig (OwnSocketConfig),
    ProvidedSocketConfig (..),
    serve,
  )
import qualified Maiwar
import Network.Simple.TCP (HostPreference (Host))
import Network.Simple.TCP.TLS (ServerParams)
import qualified Network.Simple.TCP.TLS
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    info,
    long,
    option,
    str,
    switch,
  )
import System.Environment.Blank (getEnv)
import System.Posix.Process (getProcessID)

data TLSOption = TLSOption
  { publicCertFile :: FilePath,
    privateKeyFile :: FilePath
  }
  deriving (Show)

data ListeningOption = ProvidedSocket | OwnSocket OwnSocketConfig

data Options = Options
  { tls :: Maybe TLSOption,
    listen :: ListeningOption
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional tls
    <*> (OwnSocket <$> ownSocket <|> ProvidedSocket <$ switch (long "socket-activated"))
  where
    ownSocket :: Parser OwnSocketConfig
    ownSocket =
      OwnSocketConfig
        <$> (Host <$> (option auto (long "host") <|> pure "127.0.0.1"))
        <*> option auto (long "port")

    tls :: Parser TLSOption
    tls =
      TLSOption
        <$> option str (long "public-cert-file")
        <*> option str (long "private-key-file")

tlsParams :: (MonadIO m, MonadError IOError m) => TLSOption -> m ServerParams
tlsParams tlsOption = do
  credentialResult <-
    Network.Simple.TCP.TLS.credentialLoadX509
      tlsOption.publicCertFile
      tlsOption.privateKeyFile
  case credentialResult of
    Left e -> throwError (userError e)
    Right creds -> Network.Simple.TCP.TLS.newDefaultServerParams creds

providedSocketFDs :: MonadIO m => m (Maybe [CInt])
providedSocketFDs = runMaybeT $ do
  listenPid <- read <$> MaybeT (liftIO (getEnv "LISTEN_PID"))
  listenFDs <- read @CInt <$> MaybeT (liftIO (getEnv "LISTEN_FDS"))
  myPid <- liftIO getProcessID
  guard (listenPid == myPid)
  pure [fdStart .. listenFDs - 1]
  where
    fdStart :: CInt
    fdStart = 3

providedSocketFD :: MonadIO m => m (Maybe CInt)
providedSocketFD = runMaybeT do
  fds <- MaybeT providedSocketFDs
  guard (length fds == 1)
  MaybeT (pure (listToMaybe fds))

optionsToConfig :: (MonadIO m, MonadError IOError m) => Options -> m Config
optionsToConfig options = do
  listen <- case options.listen of
    OwnSocket c -> pure (Maiwar.OwnSocket c)
    ProvidedSocket -> do
      providedSocketFDResult <- providedSocketFD
      case providedSocketFDResult of
        Nothing -> throwError (userError "No provided socket FD found")
        Just fd -> pure (Maiwar.ProvidedSocket (ProvidedSocketConfig fd))
  tls <- case options.tls of
    Nothing -> pure Nothing
    Just tlsOptions -> Just <$> tlsParams tlsOptions
  pure (Config tls listen)

defaultMain :: Handler ByteString ByteString Managed () -> IO ()
defaultMain handler = do
  options <- execParser (info optionsParser fullDesc)
  config <- optionsToConfig options
  serve config handler
