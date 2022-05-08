module Control.Monad.Managed.Extra
  ( module Control.Monad.Managed,
    around,
  )
where

import Control.Exception.Safe (bracket)
import Control.Monad.Managed

around :: MonadManaged m => IO () -> IO () -> m ()
around before after = using (managed (bracket before (const after)))
