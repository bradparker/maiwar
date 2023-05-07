{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Middleware.Static.Gzip (gzipped) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString.Char8 as BSC
import Maiwar.Handler (Request (headers), Response (body, headers))
import Maiwar.Middleware.Static.Base (StaticHandler)
import Maiwar.Network.HTTP
  ( Headers,
    alterHeader,
    findHeader,
  )
import Maiwar.Pipe.Resource (sendFile)
import qualified System.Posix.Files as Files

gzipped ::
  forall m.
  MonadResource m =>
  StaticHandler m () ->
  StaticHandler m ()
gzipped baseHandler request = runMaybeT do
  (path, response) <- MaybeT (baseHandler request)
  case findHeader "Accept-Encoding" request.headers of
    Nothing -> pure (path, response)
    Just acceptEncoding ->
      if not (BSC.isInfixOf "gzip" acceptEncoding)
        then pure (path, response)
        else do
          staticGzippedFilePathResult <- staticGzippedFilePath path
          let newResponse = case staticGzippedFilePathResult of
                Nothing -> response
                Just gzPath ->
                  let headers = addGzipContentEnconding response.headers
                   in response {headers = headers, body = sendFile 16376 gzPath}
          pure (path, newResponse)
  where
    staticGzippedFilePath :: forall n. MonadIO n => FilePath -> n (Maybe FilePath)
    staticGzippedFilePath path = do
      let gzippedFilePath = path <> ".gz"
      exists <- liftIO (Files.fileExist gzippedFilePath)
      pure
        if exists
          then Just gzippedFilePath
          else Nothing

    addGzipContentEnconding :: Headers -> Headers
    addGzipContentEnconding = alterHeader "Content-Encoding" (const (Just "gzip"))
