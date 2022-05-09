{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar
  ( Config (..),
    OwnSocketConfig (..),
    ProvidedSocketConfig (..),
    ListeningConfig (..),
    Handler,
    serve,
    respond,
    status200,
  )
where

import Control.Monad.Managed.Extra (Managed)
import Data.ByteString (ByteString)
import Maiwar.Network.HTTP (Handler, handleConnection, respond, status200)
import Maiwar.Network.TCP
  ( Config (..),
    ListeningConfig (..),
    OwnSocketConfig (..),
    ProvidedSocketConfig (..),
  )
import qualified Maiwar.Network.TCP as TCP

serve :: Config -> Handler ByteString ByteString Managed () -> IO ()
serve config handler = TCP.serve config (handleConnection handler)
