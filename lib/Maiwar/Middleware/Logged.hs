{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Middleware.Logged where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Maiwar.Handler
  ( Handler,
    Method (Method),
    HTTPVersion (HTTPVersion),
    Request (..),
    RequestTarget (RequestTarget),
    Response (..),
    Status (Status),
  )

logged ::
  forall input m.
  MonadIO m =>
  Handler input ByteString m () ->
  Handler input ByteString m ()
logged handler request = do
  start <- liftIO getCurrentTime
  response <- handler request
  liftIO do
    BSC.putStrLn
      ( clientAddress
          <> " "
          <> userIdentifier
          <> " "
          <> userId
          <> " "
          <> ("[" <> time start <> "]")
          <> " "
          <> ("\"" <> requestLine <> "\"")
          <> " "
          <> responseStatus response.status
          <> " "
          <> size
      )
  pure response
  where
    clientAddress :: ByteString
    clientAddress = BSC.pack (show request.address)

    userIdentifier :: ByteString
    userIdentifier = "-"

    userId :: ByteString
    userId = "-"

    time :: UTCTime -> ByteString
    time = BSC.pack . iso8601Show

    requestLine :: ByteString
    requestLine =
      serializeMethod request.method
        <> " "
        <> serializeTarget request.target
        <> " "
        <> serializeVersion request.httpVersion

    serializeMethod :: Method -> ByteString
    serializeMethod (Method method) = method

    serializeTarget :: RequestTarget -> ByteString
    serializeTarget (RequestTarget target) = target

    serializeVersion :: HTTPVersion -> ByteString
    serializeVersion (HTTPVersion major minor) = BSC.pack ("HTTP/" <> show major <> "." <> show minor)

    responseStatus :: Status -> ByteString
    responseStatus (Status code _) = BSC.pack (show code)

    size :: ByteString
    size = "-"
