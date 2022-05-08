{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream where

import Control.Monad (ap, when)
import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed (Managed, MonadManaged (using))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)

newtype Stream o m a
  = Stream (m (Either a (o, Stream o m a)))

newtype StreamF o m a r
  = StreamF (m (Either a (o, r)))

instance Functor m => Functor (StreamF o m r) where
  fmap :: (a -> b) -> StreamF o m r a -> StreamF o m r b
  fmap f (StreamF act) = StreamF (fmap (fmap f) <$> act)

-- ------------
-- Introduction
-- ------------

yield :: forall m a. Monad m => a -> Stream a m ()
yield = yieldM . pure

yieldM :: forall m a. Monad m => m a -> Stream a m ()
yieldM = Stream . ((Right . (,pure ())) <$>)

embed :: StreamF o m a (Stream o m a) -> Stream o m a
embed = coerce

ana :: forall a o m r. Functor m => (a -> StreamF o m r a) -> a -> Stream o m r
ana g = a where a = embed . fmap a . g

unfold :: forall a o m r. Functor m => (a -> m (Either r (o, a))) -> a -> Stream o m r
unfold f = ana (coerce . f)

-- -----------
-- Elimination
-- -----------

next :: Stream o m a -> m (Either a (o, Stream o m a))
next = coerce

cata :: forall o m a r. Functor m => (StreamF o m a r -> r) -> Stream o m a -> r
cata f = c where c = f . fmap c . coerce

fold :: forall o m a r. Functor m => (m (Either a (o, r)) -> r) -> Stream o m a -> r
fold f = cata (f . coerce)

-- | Run a stream, evaluating its effects and producing its result
run :: forall o m a. (Monad m) => Stream o m a -> m a
run = fold \action -> do
  step <- action
  case step of
    Left result -> pure result
    Right (_, rest) -> rest

-- | Empty a stream
flush :: forall o m a. (Monad m) => Stream o m a -> Stream o m a
flush = lift . run

instance forall o m. (Functor m) => Functor (Stream o m) where
  fmap :: forall a b. (a -> b) -> Stream o m a -> Stream o m b
  fmap f = Stream . fmap (bimap f (fmap (fmap f))) . next

instance forall o m. (Monad m) => Applicative (Stream o m) where
  pure :: forall a. a -> Stream o m a
  pure = Stream . pure . Left

  (<*>) :: forall a b. Stream o m (a -> b) -> Stream o m a -> Stream o m b
  (<*>) = ap

  (*>) :: forall a b. Stream o m a -> Stream o m b -> Stream o m b
  streamA *> streamB = Stream do
    step <- next streamA
    case step of
      Left _ -> next streamB
      Right layer -> pure (Right (fmap (*> streamB) layer))

instance forall o m. (Monad m) => Monad (Stream o m) where
  (>>=) :: Stream o m a -> (a -> Stream o m b) -> Stream o m b
  stream >>= k = Stream do
    step <- next stream
    case step of
      Left a -> next (k a)
      Right layer -> pure (Right (fmap (>>= k) layer))

instance forall o. MonadTrans (Stream o) where
  lift :: forall m a. Functor m => m a -> Stream o m a
  lift = Stream . fmap Left

instance forall o m. (MonadIO m) => MonadIO (Stream o m) where
  liftIO :: forall a. IO a -> Stream o m a
  liftIO = lift . liftIO

instance forall o e m. (MonadError e m) => MonadError e (Stream o m) where
  throwError :: e -> Stream f m a
  throwError = lift . throwError

  catchError :: Stream f m a -> (e -> Stream f m a) -> Stream f m a
  catchError stream catcher = Stream (next stream `catchError` (next . catcher))

instance forall o m. MonadManaged m => MonadManaged (Stream o m) where
  using :: forall a. Managed a -> Stream o m a
  using m = lift (using m)

-- --------------------
-- Transforming streams
-- --------------------

traverse :: forall a b m r. Monad m => (a -> m b) -> Stream a m r -> Stream b m r
traverse f = fold \action -> Stream do
  step <- action
  case step of
    Left result -> pure (Left result)
    Right (a, rest) -> Right . (,rest) <$> f a

filter :: forall a m r. Monad m => (a -> Bool) -> Stream a m r -> Stream a m r
filter p = fold \action -> do
  step <- lift action
  case step of
    Left result -> pure result
    Right (a, rest) -> do
      when (p a) do
        yield a
      rest
