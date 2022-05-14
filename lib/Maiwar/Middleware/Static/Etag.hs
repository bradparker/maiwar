{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Middleware.Static.Etag (etagged) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed.Extra (MonadManaged)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Maiwar.Middleware.Static.Base (StaticHandler)
import Maiwar.Network.HTTP
  ( Headers,
    Request (headers),
    Response (Response, body, headers),
    Status (Status),
    alterHeader,
    findHeader,
  )
import qualified System.Posix.Files as Files

etagged ::
  forall m.
  MonadManaged m =>
  StaticHandler m () ->
  StaticHandler m ()
etagged baseHandler request = runMaybeT do
  (path, response) <- MaybeT (baseHandler request)
  etagResult <- fetchEtag path
  let newResponse = case etagResult of
        Nothing -> response
        Just etag ->
          let headers = addEtag etag response.headers
           in case findHeader "If-None-Match" request.headers of
                Nothing -> response {headers = headers, body = response.body}
                Just reqEtag ->
                  if etag == reqEtag
                    then Response (Status 304 "Not modified") [] (pure ())
                    else response {headers = headers, body = response.body}
  pure (path, newResponse)
  where
    fetchEtag :: forall n. MonadIO n => FilePath -> n (Maybe ByteString)
    fetchEtag path = do
      let etagFilePath = path <> ".etag"
      exists <- liftIO (Files.fileExist etagFilePath)
      if exists
        then Just . BSC.strip <$> liftIO (BSC.readFile etagFilePath)
        else pure Nothing

    addEtag :: ByteString -> Headers -> Headers
    addEtag etag = alterHeader "Etag" (const (Just etag))
