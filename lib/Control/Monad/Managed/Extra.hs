{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Control.Monad.Managed.Extra
  ( module Control.Monad.Managed,
    around,
    withFile,
  )
where

import Control.Exception.Safe (bracket)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Managed
import System.IO (Handle, IOMode)
import qualified System.IO

around :: forall m. MonadManaged m => IO () -> IO () -> m ()
around before after = managed (bracket before (const after))

withFile :: forall m. MonadManaged m => FilePath -> IOMode -> m Handle
withFile path mode = managed (System.IO.withFile path mode)

instance MonadThrow Managed where
  throwM = liftIO . throwM
