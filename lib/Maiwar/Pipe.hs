{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Maiwar.Pipe where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (StateT (StateT, runStateT))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bifunctor (first)
import Maiwar.Stream (Stream (Stream), next, run, yield)

newtype Pipe i o f a
  = Pipe (forall x. StateT (Stream i f x) (Stream o f) a)

instance forall i o f. Functor f => Functor (Pipe i o f) where
  fmap :: forall a b. (a -> b) -> Pipe i o f a -> Pipe i o f b
  fmap f (Pipe sa) = Pipe (fmap f sa)

instance forall i o f. Monad f => Applicative (Pipe i o f) where
  pure :: a -> Pipe i o f a
  pure a = Pipe (pure a)

  (*>) :: Pipe i o f a -> Pipe i o f b -> Pipe i o f b
  Pipe as *> Pipe bs = Pipe (as *> bs)

  (<*>) :: Pipe i o f (a -> b) -> Pipe i o f a -> Pipe i o f b
  Pipe fs <*> Pipe as = Pipe (fs <*> as)

instance forall i o f. Monad f => Monad (Pipe i o f) where
  (>>=) :: forall a b. Pipe i o f a -> (a -> Pipe i o f b) -> Pipe i o f b
  Pipe p >>= k = Pipe (p >>= (\a -> case k a of Pipe q -> q))

instance forall i o f. MonadFail f => MonadFail (Pipe i o f) where
  fail :: String -> Pipe i o f a
  fail = lift . fail

instance forall i o. MonadTrans (Pipe i o) where
  lift :: forall f a. Monad f => f a -> Pipe i o f a
  lift fa = Pipe (lift (lift fa))

instance forall i o f. MonadIO f => MonadIO (Pipe i o f) where
  liftIO :: forall a. IO a -> Pipe i o f a
  liftIO = lift . liftIO

runPipe ::
  forall i o f a b.
  Pipe i o f b ->
  Stream i f a ->
  Stream o f (b, Stream i f a)
runPipe (Pipe s) = runStateT s

execPipe ::
  forall i o f a b.
  Functor f =>
  Pipe i o f b ->
  Stream i f a ->
  Stream o f (Stream i f a)
execPipe p s = fmap snd (runPipe p s)

receive :: forall i o f. Monad f => Pipe i o f (Maybe i)
receive = Pipe $ StateT \input -> Stream do
  step <- next input
  case step of
    Left a -> pure (Left (Nothing, pure a))
    Right (i, rest) -> pure (Left (Just i, rest))

send :: forall i o f. Monad f => o -> Pipe i o f ()
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
-- >>> a = replicateM_ 5 (pipe (\n -> replicate n 'a'))
-- >>> b = replicateM_ 6 (pipe (* 2))
-- >>> c = replicateM_ 7 (pipe (+ 1))
-- >>> d = a `compose` (b `compose` c)
-- >>> run (for (void (runPipe d (forever (yield 3)))) (liftIO . print))
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- >>> e = (a `compose` b) `compose` c
-- >>> run (for (void (runPipe e (forever (yield 3)))) (liftIO . print))
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
compose :: forall m a b c r. Monad m => Pipe b c m r -> Pipe a b m r -> Pipe a c m r
compose pipeA pipeB =
  Pipe
    ( StateT
        ( \input -> do
            (r, s) <- runPipe pipeA (runPipe pipeB input)
            first (const r) <$> lift (run s)
        )
    )

type Consumer i f a = forall x. Pipe i x f a

subState ::
  forall f s t a.
  Monad f =>
  (s -> t) ->
  (t -> s) ->
  StateT t f a ->
  StateT s f a
subState i o st = StateT (fmap (fmap o) . runStateT st . i)

subInput ::
  forall i o m n a.
  Monad m =>
  (forall x. Stream i m x -> Stream i m (n x)) ->
  (forall x. Stream i m (n x) -> Stream i m x) ->
  Pipe i o m a ->
  Pipe i o m a
subInput i o (Pipe b) = Pipe (subState i o b)
