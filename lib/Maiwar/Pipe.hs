{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Pipe where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed (Managed, MonadManaged (using))
import Control.Monad.State (StateT (StateT, runStateT), modify)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bifunctor (first)
import Maiwar.Stream (Stream (Stream), next, run, yield)
import qualified System.IO
import Prelude hiding (filter, map, print)

newtype Pipe i o m a
  = Pipe (forall x. StateT (Stream i m x) (Stream o m) a)

-- ------------
-- Introduction
-- ------------

-- | Construct a Pipe from a StateT-like function
pipe :: forall i o m a. (forall x. Stream i m x -> Stream o m (a, Stream i m x)) -> Pipe i o m a
pipe f = Pipe (StateT f)

-- | Receive input from upstream
-- If the input stream is exhausted, `Nothing` is returned.
receive :: forall i o m. Monad m => Pipe i o m (Maybe i)
receive =
  Pipe
    ( StateT \input -> Stream do
        step <- next input
        case step of
          Left a -> pure (Left (Nothing, pure a))
          Right (i, rest) -> pure (Left (Just i, rest))
    )

-- | Replace received input
replace :: forall i o m. Monad m => i -> Pipe i o m ()
replace i = Pipe (modify (yield i *>))

-- | Send output down stream
send :: forall i o m. Monad m => o -> Pipe i o m ()
send o = Pipe (lift (yield o))

-- ------------
-- Elimination
-- ------------

-- | Run a pipe, returning both its result and any unconsumed input
runPipe ::
  forall i o m a b.
  Pipe i o m b ->
  Stream i m a ->
  Stream o m (b, Stream i m a)
runPipe (Pipe s) = runStateT s

-- | Run a pipe, ignoring its result and returning any unconsumed input
execPipe ::
  forall i o m a b.
  Functor m =>
  Pipe i o m b ->
  Stream i m a ->
  Stream o m (Stream i m a)
execPipe p s = snd <$> runPipe p s

-- | Run a pipe, returning its result and ignoring any unconsumed input
evalPipe ::
  forall i o m a b.
  Functor m =>
  Pipe i o m b ->
  Stream i m a ->
  Stream o m b
evalPipe p s = fst <$> runPipe p s

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

instance forall i o m. MonadManaged m => MonadManaged (Pipe i o m) where
  using :: forall a. Managed a -> Pipe i o m a
  using m = lift (using m)

instance forall i o m a. (Monad m, Semigroup a) => Semigroup (Pipe i o m a) where
  (<>) :: Pipe i o m a -> Pipe i o m a -> Pipe i o m a
  a <> b = (<>) <$> a <*> b

instance forall i o m a. (Monad m, Monoid a) => Monoid (Pipe i o m a) where
  mempty :: Pipe i o m a
  mempty = pure mempty

-- ------------------
-- Pipe composition
-- ------------------

-- | Compose
-- >>> import qualified Maiwar.Stream as Stream
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
-- >>> run (evalPipe (d >- print) (forever (yield 3)))
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- 'A'
-- >>> e = (a `compose` b) `compose` c
-- >>> run (evalPipe (e >- print) (forever (yield 3)))
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- "aaaaaaaa"
-- 'A'
-- >>> f = b `compose` c
-- >>> run (evalPipe (f >- print) (forever (yield 3)))
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

-- | A composition operator
-- Nicked from Pipes (https://hackage.haskell.org/package/pipes-4.3.16/docs/Pipes.html#v:-60--45--60-)
(<-<) :: forall m a b c r s. Monad m => Pipe b c m r -> Pipe a b m s -> Pipe a c m r
(<-<) = compose

infixr 1 <-<

(>->) :: forall m a b c r s. Monad m => Pipe a b m s -> Pipe b c m r -> Pipe a c m r
(>->) = flip compose

infixr 1 >->

-- ---------
-- Utilities
-- ---------

subState ::
  forall m s t a.
  Monad m =>
  (s -> t) ->
  (t -> s) ->
  StateT t m a ->
  StateT s m a
subState i o st = StateT (fmap (fmap o) . runStateT st . i)

-- | Transform the input stream over which a pipe operates
subInput ::
  forall i o m n a.
  Monad m =>
  (forall x. Stream i m x -> Stream i m (n x)) ->
  (forall x. Stream i m (n x) -> Stream i m x) ->
  Pipe i o m a ->
  Pipe i o m a
subInput i o (Pipe b) = Pipe (subState i o b)

-- -------------------
-- Pipes as operations
-- -------------------

-- | Filter
filter :: Monad m => (a -> Bool) -> Pipe a a m ()
filter p = go
  where
    go = do
      input <- receive
      case input of
        Nothing -> pure ()
        Just a -> do
          when (p a) do
            send a
          go

-- | Map
-- >>> run (evalPipe (map (+ 1) *> send 4 >- print) (yield 1 *> yield 2))
-- 2
-- 3
-- 4
map :: forall a b m. Monad m => (a -> b) -> Pipe a b m ()
map f = go
  where
    go = do
      input <- receive
      case input of
        Nothing -> pure ()
        Just a -> do
          send (f a)
          go

fold :: forall a m. (Monoid a, Monad m) => Pipe a a m a
fold = go mempty
  where
    go acc = do
      input <- receive
      case input of
        Nothing -> pure acc
        Just a -> go (acc <> a)

-- ---------
-- Consumers
-- ---------

type Consumer i m a = forall o. Pipe i o m a

-- | Consume the items in a pipe, preserving its output
consume :: forall a b m r s. Monad m => Pipe a b m r -> Consumer b m s -> Consumer a m r
consume p consumer =
  Pipe
    ( StateT \input -> do
        (_, s) <- runPipe consumer (runPipe p input)
        lift (run s)
    )

(>-) :: forall a b m r s. Monad m => Pipe a b m r -> Consumer b m s -> Consumer a m r
(>-) = consume

infixr 1 >-

-- | Consume input by printing it to the console
print :: (Show a, MonadIO m) => Consumer a m ()
print = go
  where
    go = do
      input <- receive
      case input of
        Nothing -> pure ()
        Just a -> do
          liftIO (System.IO.print a)
          go
