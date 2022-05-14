{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Pipe.Managed where

import Control.Monad.Managed.Extra (MonadManaged)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Maiwar.Pipe (Pipe, pipe)
import qualified Maiwar.Stream.Managed

sendFile :: forall a m. MonadManaged m => Int -> FilePath -> Pipe a ByteString m ()
sendFile chunkSize path = pipe (\s -> Maiwar.Stream.Managed.readFile chunkSize path $> ((), s))
