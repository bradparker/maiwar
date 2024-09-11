{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Maiwar.MSF where

import Control.Arrow (Arrow (arr, (***)), ArrowLoop (loop), first)
import Control.Category (Category (id, (.)))
import Control.Monad.Fix (MonadFix)
import Data.Bifunctor (bimap)
import Prelude hiding (id, (.))

newtype MSF m i o = MSF (i -> m (o, MSF m i o))

arrM :: forall m i o. Monad m => (i -> m o) -> MSF m i o
arrM f = MSF \i -> do
  o <- f i
  pure (o, arrM f)

embed :: forall m i o. Monad m => MSF m i o -> [i] -> m [o]
embed (MSF msf) [] = pure []
embed (MSF msf) (i : is) = do
  (o, msf') <- msf i
  (o :) <$> embed msf' is

feedback :: forall m a b c. Monad m => c -> MSF m (a, c) (b, c) -> MSF m a b
feedback c (MSF msf) = MSF \a -> do
  ((b, c'), msf') <- msf (a, c)
  pure (b, feedback c' msf')

instance forall m i. (Functor m) => Functor (MSF m i) where
  fmap :: (a -> b) -> MSF m i a -> MSF m i b
  fmap f (MSF msf) = MSF (fmap (bimap f (fmap f)) . msf)

instance forall m i. (Applicative m) => Applicative (MSF m i) where
  pure :: a -> MSF m i a
  pure a = MSF (\_ -> pure (a, pure a))

  (<*>) :: MSF m i (a -> b) -> MSF m i a -> MSF m i b
  MSF msff <*> MSF msfa = MSF \i ->
    (\(f, msff') (a, msfa') -> (f a, msff' <*> msfa')) <$> msff i <*> msfa i

instance forall m. (Monad m) => Category (MSF m) where
  id :: forall a. MSF m a a
  id = MSF \a -> pure (a, id)

  (.) :: forall a b c. MSF m b c -> MSF m a b -> MSF m a c
  MSF f . MSF g = MSF \a -> do
    (b, g') <- g a
    fmap (. g') <$> f b

instance forall m. (Monad m) => Arrow (MSF m) where
  arr :: (a -> b) -> MSF m a b
  arr f = MSF \a -> pure (f a, arr f)

  (***) :: forall a b c d. MSF m a b -> MSF m c d -> MSF m (a, c) (b, d)
  MSF f *** MSF g = MSF \(a, c) ->
    (\(b, f') (d, g') -> ((b, d), f' *** g')) <$> f a <*> g c

instance forall m. (MonadFix m) => ArrowLoop (MSF m) where
  loop :: forall a b c. MSF m (a, c) (b, c) -> MSF m a b
  loop (MSF msf) = MSF \a -> do
    rec ((b, c), msf') <- msf (a, c)
    pure (b, loop msf')
