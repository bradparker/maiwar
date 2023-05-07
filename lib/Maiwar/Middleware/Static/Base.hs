{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Middleware.Static.Base
  ( baseHandler,
    StaticHandler,
  )
where

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.IO.Exception (IOErrorType (NoSuchThing), IOException (ioe_type))
import Maiwar.Handler
  ( Request (target),
    RequestTarget (RequestTarget),
    Response (Response),
    status200,
  )
import Maiwar.Network.HTTP (Headers, alterHeader)
import Maiwar.Pipe (Consumer, Pipe)
import Maiwar.Pipe.Resource (sendFile)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import System.Posix.Files (FileStatus)
import qualified System.Posix.Files as Files

type StaticHandler m a =
  forall input.
  Request ->
  Consumer input m (Maybe (FilePath, Response (Pipe input ByteString m a)))

baseHandler ::
  forall m.
  MonadResource m =>
  FilePath ->
  StaticHandler m ()
baseHandler directory request = do
  staticPathResult <- staticFilePath
  case staticPathResult of
    Nothing -> pure Nothing
    Just path ->
      let headers = addContentType path []
       in pure (Just (path, Response status200 headers (sendFile 16376 path)))
  where
    staticFilePath :: forall n. MonadIO n => n (Maybe FilePath)
    staticFilePath = runMaybeT do
      target <- (directory </>) <$> safeTarget
      status <- fileStatus target
      pure
        if Files.isDirectory status
          then target </> "index.html"
          else target
      where
        safeTarget :: MaybeT n FilePath
        safeTarget = (MaybeT . pure) case request.target of
          RequestTarget bytes ->
            if BSC.isInfixOf ".." bytes
              then Nothing
              else Just (BSC.unpack (BSC.dropWhile (== '/') bytes))

        fileStatus :: FilePath -> MaybeT n FileStatus
        fileStatus path = (MaybeT . liftIO) do
          (Just <$> Files.getFileStatus path) `catchError` \(e :: IOException) ->
            case e.ioe_type of
              NoSuchThing -> pure Nothing
              _ -> throwError e

    addContentType :: FilePath -> Headers -> Headers
    addContentType path = alterHeader "Content-Type" (const (mimeType path))

    -- TODO: consider using https://hackage.haskell.org/package/mime-types
    mimeType :: FilePath -> Maybe ByteString
    mimeType path =
      Map.lookup (FilePath.takeExtension path) mimeTypes

    mimeTypes :: Map String ByteString
    mimeTypes =
      Map.fromList
        [ (".css", "text/css"),
          (".eot", "application/vnd.ms-fontobject"),
          (".gif", "image/gif"),
          (".html", "text/html"),
          (".jpg", "image/jpeg"),
          (".png", "image/png"),
          (".pdf", "application/pdf"),
          (".svg", "image/svg+xml"),
          (".ttf", "font/ttf"),
          (".woff", "font/woff"),
          (".woff2", "font/woff2")
        ]
