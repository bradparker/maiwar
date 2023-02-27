{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Middleware.Redirecting where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Maiwar.Handler
  ( Handler,
    Request (..),
    RequestTarget (RequestTarget),
    respond,
    status301,
    (=:),
  )

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
