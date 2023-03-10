{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Middleware.Redirecting where

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
  forall m body.
  (Monad m, Monoid body) =>
  Map RequestTarget RequestTarget ->
  Handler m body ->
  Handler m body
redirecting locationMap handler request =
  case Map.lookup request.target locationMap of
    Nothing -> handler request
    Just (RequestTarget newTarget) ->
      respond status301 ["Location" =: newTarget] mempty
