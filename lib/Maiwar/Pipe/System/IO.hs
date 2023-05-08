{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Pipe.System.IO where

import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Maiwar.Pipe (Pipe, pipe)
import qualified Maiwar.Stream.System.IO as Stream

sendFile :: forall a m. MonadResource m => Int -> FilePath -> Pipe a ByteString m ()
sendFile chunkSize path = pipe (\s -> Stream.readFile chunkSize path $> ((), s))
