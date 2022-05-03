{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Pipe where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (StateT (StateT, runStateT))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bifunctor (first)
import Maiwar.Stream (Stream (Stream), next, run, yield)

newtype Pipe i o m a
  = Pipe (forall x. StateT (Stream i m x) (Stream o m) a)

instance forall i o m. Functor m => Functor (Pipe i o m) where
  fmap :: forall a b. (a -> b) -> Pipe i o m a -> Pipe i o m b
  fmap f (Pipe sa) = Pipe (fmap f sa)

instance forall i o m. Monad m => Applicative (Pipe i o m) where
  pure :: a -> Pipe i o m a
  pure a = Pipe (pure a)

  (*>) :: Pipe i o m a -> Pipe i o m b -> Pipe i o m b
  Pipe as *> Pipe bs = Pipe (as *> bs)

  (<*>) :: Pipe i o m (a -> b) -> Pipe i o m a -> Pipe i o m b
  Pipe fs <*> Pipe as = Pipe (fs <*> as)

instance forall i o m. Monad m => Monad (Pipe i o m) where
  (>>=) :: forall a b. Pipe i o m a -> (a -> Pipe i o m b) -> Pipe i o m b
  Pipe p >>= k = Pipe (p >>= (\a -> case k a of Pipe q -> q))

instance forall i o m. MonadFail m => MonadFail (Pipe i o m) where
  fail :: String -> Pipe i o m a
  fail = lift . fail

instance forall i o. MonadTrans (Pipe i o) where
  lift :: forall m a. Monad m => m a -> Pipe i o m a
  lift fa = Pipe (lift (lift fa))

instance forall i o m. MonadIO m => MonadIO (Pipe i o m) where
  liftIO :: forall a. IO a -> Pipe i o m a
  liftIO = lift . liftIO

runPipe ::
  forall i o m a b.
  Pipe i o m b ->
  Stream i m a ->
  Stream o m (b, Stream i m a)
runPipe (Pipe s) = runStateT s

execPipe ::
  forall i o m a b.
  Functor m =>
  Pipe i o m b ->
  Stream i m a ->
  Stream o m (Stream i m a)
execPipe p s = fmap snd (runPipe p s)

receive :: forall i o m. Monad m => Pipe i o m (Maybe i)
receive = Pipe $ StateT \input -> Stream do
  step <- next input
  case step of
    Left a -> pure (Left (Nothing, pure a))
    Right (i, rest) -> pure (Left (Just i, rest))

send :: forall i o m. Monad m => o -> Pipe i o m ()
send o = Pipe (lift (yield o))

-- | Compose
--
-- >>> import Maiwar.Stream (for)
-- >>> import Data.Functor (void)
-- >>> import Control.Monad (replicateM_, forever)
-- >>> :{
--   pipe :: forall m a b. Monad m => (a -> b) -> Pipe a b m ()
--   pipe f = do
--     input <- receive
--     case input of
--       Nothing -> pure ()
--       Just a -> send (f a)
-- :}
--
-- >>> a = replicateM_ 5 (pipe (\n -> replicate n 'a')) *> pure 'A'
-- >>> b = replicateM_ 6 (pipe (* 2)) *> pure 'B'
-- >>> c = replicateM_ 7 (pipe (+ 1)) *> pure 'C'
-- >>> d = a `compose` (b `compose` c)
-- >>> fst <$> run (for (runPipe d (forever (yield 3))) (liftIO . print))
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- 'A'
-- >>> e = (a `compose` b) `compose` c
-- >>> fst <$> run (for (runPipe e (forever (yield 3))) (liftIO . print))
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- 'A'
-- >>> f = b `compose` c
-- >>> fst <$> run (for (runPipe f (forever (yield 3))) (liftIO . print))
-- 8
-- 8
-- 8
-- 8
-- 8
-- 8
-- 'B'
compose :: forall m a b c r s. Monad m => Pipe b c m r -> Pipe a b m s -> Pipe a c m r
compose pipeA pipeB =
  Pipe
    ( StateT
        ( \input -> do
            (r, s) <- runPipe pipeA (runPipe pipeB input)
            first (const r) <$> lift (run s)
        )
    )

type Consumer i m a = forall x. Pipe i x m a

subState ::
  forall m s t a.
  Monad m =>
  (s -> t) ->
  (t -> s) ->
  StateT t m a ->
  StateT s m a
subState i o st = StateT (fmap (fmap o) . runStateT st . i)

subInput ::
  forall i o m n a.
  Monad m =>
  (forall x. Stream i m x -> Stream i m (n x)) ->
  (forall x. Stream i m (n x) -> Stream i m x) ->
  Pipe i o m a ->
  Pipe i o m a
subInput i o (Pipe b) = Pipe (subState i o b)
