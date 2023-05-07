{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Pipe.Resource where

import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Maiwar.Pipe (Pipe, pipe)
import qualified Maiwar.Stream.Resource

sendFile :: forall a m. MonadResource m => Int -> FilePath -> Pipe a ByteString m ()
sendFile chunkSize path = pipe (\s -> Maiwar.Stream.Resource.readFile chunkSize path $> ((), s))
