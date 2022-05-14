{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Managed.Extra
  ( module Control.Monad.Managed,
    around,
    withFile,
  )
where

import Control.Exception.Safe (bracket)
import Control.Monad.Managed
import System.IO (Handle, IOMode)
import qualified System.IO

around :: forall m. MonadManaged m => IO () -> IO () -> m ()
around before after = using (managed (bracket before (const after)))

withFile :: forall m. MonadManaged m => FilePath -> IOMode -> m Handle
withFile path mode = using (managed (System.IO.withFile path mode))
