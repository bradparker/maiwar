{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Handlers.Static (static) where

import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Maiwar.Handler (Method (Method), Request (method), StreamingHandler, respond)
import qualified Maiwar.Middleware.Static as Middleware
import Maiwar.Network.HTTP (status404, status405, (=:))
import Maiwar.Pipe.Resource (sendFile)
import System.FilePath ((</>))

static ::
  forall input m.
  MonadResource m =>
  FilePath ->
  StreamingHandler input ByteString m ()
static directory = Middleware.static directory \request ->
  case request.method of
    Method "GET" -> do
      respond status404 ["Content-Type" =: "text/html"] do
        sendFile 16376 (directory </> "404.html")
    Method _ -> do
      respond status405 [] (pure ())
