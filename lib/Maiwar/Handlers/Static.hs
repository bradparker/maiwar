{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Handlers.Static (static) where

import Control.Monad.Managed.Extra (MonadManaged)
import Data.ByteString (ByteString)
import Maiwar.Handler (Handler, Method (Method), Request (method), respond)
import qualified Maiwar.Middleware.Static as Middleware
import Maiwar.Network.HTTP (status404, status405, (=:))
import Maiwar.Pipe.Managed (sendFile)
import System.FilePath ((</>))

static ::
  forall input m.
  MonadManaged m =>
  FilePath ->
  Handler input ByteString m ()
static directory = Middleware.static directory \request ->
  case request.method of
    Method "GET" -> do
      respond status404 ["Content-Type" =: "text/html"] do
        sendFile 16376 (directory </> "404.html")
    Method _ -> do
      respond status405 [] (pure ())
