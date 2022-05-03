{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream.ByteString where

import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Maiwar.Stream (Stream, next, yield)
import Prelude hiding (drop, splitAt)

splitAt ::
  forall m a.
  Monad m =>
  Int ->
  Stream ByteString m a ->
  Stream ByteString m (Stream ByteString m a)
splitAt n stream
  | n <= 0 = pure stream
  | otherwise = do
      step <- lift (next stream)
      case step of
        Left a -> pure (pure a)
        Right (bytes, rest) ->
          if BS.length bytes > n
            then do
              yield (BS.take n bytes)
              pure (yield (BS.drop n bytes) *> rest)
            else do
              yield bytes
              splitAt (n - BS.length bytes) rest

drop ::
  forall m a.
  Monad m =>
  Int ->
  Stream ByteString m a ->
  Stream ByteString m a
drop n stream
  | n <= 0 = stream
  | otherwise = do
      step <- lift (next stream)
      case step of
        Left a -> pure a
        Right (bytes, rest) -> do
          if n < BS.length bytes
            then do
              yield (BS.drop n bytes)
              rest
            else drop (n - BS.length bytes) rest
