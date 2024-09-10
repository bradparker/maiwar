{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream.Exception where

import Control.Exception (Exception)
import qualified Control.Exception.Safe as Exception
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Maiwar.Stream (Stream, fold, yield)

try :: forall e o m a. (MonadCatch m, Exception e) => Stream o m a -> Stream o m (Either e a)
try = fold \action -> do
  step <- lift (Exception.try action)
  case step of
    Left e -> pure (Left e)
    Right (Left a) -> pure (Right a)
    Right (Right (o, rest)) -> do
      yield o
      rest

tryAsync :: forall e o m a. (MonadCatch m, Exception e) => Stream o m a -> Stream o m (Either e a)
tryAsync = fold \action -> do
  step <- lift (Exception.tryAsync action)
  case step of
    Left e -> pure (Left e)
    Right (Left a) -> pure (Right a)
    Right (Right (o, rest)) -> do
      yield o
      rest
