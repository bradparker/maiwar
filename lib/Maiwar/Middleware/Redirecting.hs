{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Middleware.Redirecting where

import Data.ByteString (ByteString)
import Maiwar.Handler
  ( Handler,
    Request (..),
    RequestTarget (RequestTarget),
    status301,
    respond,
    (=:),
  )
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

redirecting ::
  forall input m.
  Monad m =>
  Map RequestTarget RequestTarget ->
  Handler input ByteString m () ->
  Handler input ByteString m ()
redirecting locationMap handler request =
  case Map.lookup request.target locationMap of
    Nothing -> handler request
    Just (RequestTarget newTarget) ->
      respond status301 ["Location" =: newTarget] (pure ())
