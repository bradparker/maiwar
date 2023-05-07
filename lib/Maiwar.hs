{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar
  ( Config (..),
    OwnSocketConfig (..),
    ProvidedSocketConfig (..),
    ListeningConfig (..),
    Handler,
    StreamingHandler,
    serve,
    respond,
    status200,
  )
where

import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString (ByteString)
import Maiwar.Handler (Handler, StreamingHandler, handleConnection, respond, status200)
import Maiwar.Network.TCP
  ( Config (..),
    ListeningConfig (..),
    OwnSocketConfig (..),
    ProvidedSocketConfig (..),
  )
import qualified Maiwar.Network.TCP as TCP

serve :: Config -> StreamingHandler ByteString ByteString (ResourceT IO) () -> IO ()
serve config handler = TCP.serve config (handleConnection handler)
