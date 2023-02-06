{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Middleware.Static where

import Control.Monad.Managed.Extra (MonadManaged)
import Data.ByteString (ByteString)
import Maiwar.Handler (Handler, Method (Method), Request (method), Response (body))
import Maiwar.Middleware.Static.Base (baseHandler)
import Maiwar.Middleware.Static.Etag (etagged)
import Maiwar.Middleware.Static.Gzip (gzipped)

static ::
  forall input m.
  MonadManaged m =>
  FilePath ->
  Handler input ByteString m () ->
  Handler input ByteString m ()
static directory fallback =
  let handler = etagged (gzipped (baseHandler directory))
   in \request -> do
        case request.method of
          Method "GET" -> do
            result <- handler request
            case result of
              Nothing -> fallback request
              Just (_, response) -> pure response
          Method "HEAD" -> do
            result <- handler request
            case result of
              Nothing -> fallback request
              Just (_, response) -> pure response {body = pure ()}
          _ -> fallback request
