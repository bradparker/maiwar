{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Handler
  ( Handler,
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

import Data.ByteString (ByteString)
import Maiwar.Network.HTTP
  ( HTTPVersion (..),
    Method (..),
    RequestTarget (..),
    Response (..),
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

type Handler input output m result =
  Request -> Consumer input m (Response (Pipe input output m result))

respond ::
  forall m body.
  Applicative m =>
  HTTP.Status ->
  HTTP.Headers ->
  body ->
  m (Response body)
respond status headers = pure . Response status headers

toHTTPHandler ::
  forall m.
  Monad m =>
  Handler ByteString ByteString m () ->
  SockAddr ->
  HTTP.Handler ByteString ByteString m ()
toHTTPHandler handler addr =
  handler . requestFromHTTPRequest addr

handleConnection ::
  forall m.
  Monad m =>
  Handler ByteString ByteString m () ->
  SockAddr ->
  Pipe ByteString ByteString m ()
handleConnection handler addr =
  HTTP.handleConnection (toHTTPHandler handler addr)
