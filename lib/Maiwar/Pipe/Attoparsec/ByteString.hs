{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Pipe.Attoparsec.ByteString where

import Data.Attoparsec.ByteString (IResult (Done, Fail, Partial), Parser)
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.List as List
import Maiwar.Pipe (Consumer, receive, replace)

-- | Use a Pipe as input to a resumable Attoparsec parser
--
-- >>> import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
-- >>> import Maiwar.Stream (yield, run)
-- >>> import Maiwar.Pipe (evalPipe)
-- >>> run (evalPipe (parse (Attoparsec.decimal <* Attoparsec.endOfInput)) (yield "123" *> yield "456"))
-- Right 123456
parse :: forall a m. Monad m => Parser a -> Consumer ByteString m (Either String a)
parse parser = do
  input <- receive
  case input of
    Nothing -> case Attoparsec.parseOnly parser "" of
      Left message -> pure (Left message)
      Right a -> pure (Right a)
    Just bytes -> go (Attoparsec.parse parser bytes)
  where
    go result =
      case result of
        Fail remaining [] message -> do
          replace remaining
          pure (Left message)
        Fail remaining contexts message -> do
          replace remaining
          pure (Left (List.intercalate " > " contexts <> ": " <> message))
        Done remaining a -> do
          replace remaining
          pure (Right a)
        Partial cont -> do
          input <- receive
          case input of
            Nothing -> case Attoparsec.eitherResult (cont "") of
              Left message -> pure (Left message)
              Right a -> pure (Right a)
            Just bytes -> go (cont bytes)
