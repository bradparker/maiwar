{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream.Managed where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed.Extra (MonadManaged, withFile)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Maiwar.Stream (Stream, unfold)
import System.IO (IOMode (ReadMode))
import qualified System.IO

readFile :: forall m. MonadManaged m => Int -> FilePath -> Stream ByteString m ()
readFile chunkSize path =
  withFile path ReadMode >>= unfold \handle ->
    liftIO do
      bytes <- BSC.hGet handle chunkSize
      if BSC.null bytes
        then Left <$> System.IO.hClose handle
        else pure (Right (bytes, handle))
