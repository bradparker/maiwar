{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Handler
  ( Handler,
    StreamingHandler,
    SimpleHandler,
    handleConnection,
    HTTPVersion (..),
    Request (..),
    Method (..),
    RequestTarget (..),
    Response (..),
    Status (..),
    status200,
    status301,
    status400,
    respond,
    (=:),
  )
where

import Control.Applicative (Alternative)
import Control.Exception.Safe (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Maiwar.Network.HTTP
  ( HTTPVersion (..),
    Headers,
    Method (..),
    RequestTarget (..),
    Status (..),
    status200,
    status301,
    status400,
    (=:),
  )
import qualified Maiwar.Network.HTTP as HTTP
import Maiwar.Pipe (Consumer, Pipe)
import Network.Simple.TCP (SockAddr)

data Request = Request
  { address :: SockAddr,
    method :: HTTP.Method,
    target :: HTTP.RequestTarget,
    httpVersion :: HTTPVersion,
    headers :: HTTP.Headers
  }
  deriving (Show)

requestFromHTTPRequest :: SockAddr -> HTTP.Request -> Request
requestFromHTTPRequest
  addr
  ( HTTP.Request
      method
      target
      httpVersion
      headers
    ) =
    Request
      addr
      method
      target
      httpVersion
      headers

data Response body = Response
  { status :: Status,
    headers :: Headers,
    body :: body
  }

httpResponseFromResponse :: Response (Pipe input output m r) -> HTTP.Response input output m r
httpResponseFromResponse response =
  HTTP.Response
    response.status
    response.headers
    response.body

type Handler m body =
  Request -> m (Response body)

type StreamingHandler input output m result =
  Handler (Consumer input m) (Pipe input output m result)

type SimpleHandler input m output =
  Handler (ReaderT input m) output

respond ::
  forall m body.
  (Applicative m) =>
  HTTP.Status ->
  HTTP.Headers ->
  body ->
  m (Response body)
respond status headers = pure . Response status headers

toHTTPHandler ::
  forall m.
  (Monad m) =>
  StreamingHandler ByteString ByteString m () ->
  SockAddr ->
  HTTP.Handler m ()
toHTTPHandler handler addr =
  (httpResponseFromResponse <$>) . handler . requestFromHTTPRequest addr

handleConnection ::
  forall m.
  (MonadCatch m, MonadIO m, Alternative m) =>
  StreamingHandler ByteString ByteString m () ->
  SockAddr ->
  Pipe ByteString ByteString m ()
handleConnection handler addr =
  HTTP.handleConnection (toHTTPHandler handler addr)
