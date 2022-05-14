{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Handlers.Static (static) where

import Control.Monad.Managed.Extra (MonadManaged)
import Data.ByteString (ByteString)
import qualified Maiwar.Middleware.Static as Middleware
import Maiwar.Network.HTTP (Handler, respond, status404, (=:))
import Maiwar.Pipe.Managed (sendFile)
import System.FilePath ((</>))

static ::
  forall input m.
  MonadManaged m =>
  FilePath ->
  Handler input ByteString m ()
static directory = Middleware.static directory \_ ->
  respond status404 ["Content-Type" =: "text/html"] do
    sendFile 16376 (directory </> "404.html")
