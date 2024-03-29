{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream.System.IO where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (MonadResource, allocate, release)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Maiwar.Stream (Stream, yield)
import System.IO (IOMode (ReadMode))
import qualified System.IO

readFile :: forall m. MonadResource m => Int -> FilePath -> Stream ByteString m ()
readFile chunkSize path = do
  (releaseKey, handle) <- allocate (System.IO.openFile path ReadMode) System.IO.hClose
  let loop = do
        bytes <- liftIO (BSC.hGet handle chunkSize)
        if BSC.null bytes
          then pure ()
          else do
            yield bytes
            loop
  loop
  release releaseKey
