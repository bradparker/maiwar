{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Maiwar.Stream where

import Control.Monad (ap)
import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)

newtype Stream o m a
  = Stream (m (Either a (o, Stream o m a)))

next :: Stream o m a -> m (Either a (o, Stream o m a))
next (Stream s) = s

yield :: forall m a. Monad m => a -> Stream a m ()
yield = yieldM . pure

yieldM :: forall m a. Monad m => m a -> Stream a m ()
yieldM = Stream . ((Right . (,pure ())) <$>)

newtype StreamF o m a r
  = StreamF (m (Either a (o, r)))

instance Functor m => Functor (StreamF o m r) where
  fmap :: (a -> b) -> StreamF o m r a -> StreamF o m r b
  fmap f (StreamF act) = StreamF (fmap (fmap f) <$> act)

project :: Stream o m a -> StreamF o m a (Stream o m a)
project = coerce

embed :: StreamF o m a (Stream o m a) -> Stream o m a
embed = coerce

cata :: forall o m a r. Functor m => (StreamF o m a r -> r) -> Stream o m a -> r
cata f = c where c = f . fmap c . project

fold :: forall o m a r. Functor m => (m (Either a (o, r)) -> r) -> Stream o m a -> r
fold f = cata (\(StreamF action) -> f action)

ana :: forall a o m r. Functor m => (a -> StreamF o m r a) -> a -> Stream o m r
ana g = a where a = embed . fmap a . g

unfold :: forall a o m r. Functor m => (a -> m (Either r (o, a))) -> a -> Stream o m r
unfold f = ana (StreamF . f)

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

for :: forall a b m r. Monad m => Stream a m r -> (a -> Stream b m r) -> Stream b m r
for stream f = (`fold` stream) \action -> do
  step1 <- lift action
  case step1 of
    Left result -> pure result
    Right (a, as) -> do
      step2 <- lift (next (f a))
      case step2 of
        Left _ -> as
        Right (b, bs) -> do
          yield b
          bs *> as

-- | Run a stream, evaluating its effects and producing its result
-- >>> :{
-- run do
--   lift (putStrLn "Hello,")
--   lift (putStrLn "Streams")
--   pure "Done!"
-- :}
-- Hello,
-- Streams
-- "Done!"
run :: forall o m a. (Monad m) => Stream o m a -> m a
run = fold \action -> do
  step <- action
  case step of
    Left result -> pure result
    Right (_, rest) -> rest

flush :: forall o m a. (Monad m) => Stream o m a -> Stream o m a
flush = lift . run
