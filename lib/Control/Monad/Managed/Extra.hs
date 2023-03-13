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
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.Managed
import System.IO (Handle, IOMode)
import qualified System.IO

around :: forall m. MonadManaged m => IO () -> IO () -> m ()
around before after = managed (bracket before (const after))

withFile :: forall m. MonadManaged m => FilePath -> IOMode -> m Handle
withFile path mode = managed (System.IO.withFile path mode)

instance MonadThrow Managed where
  throwM = liftIO . throwM

instance MonadCatch Managed where
  catch m catcher = managed \return_ -> do
    with m return_ `catch` (flip with return_ . catcher)
