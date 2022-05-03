{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Stream.Attoparsec.ByteString where

import Data.Attoparsec.ByteString (IResult (Done, Fail, Partial), Parser, Result)
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.List as List
import Maiwar.Stream (Stream, next, yield)

-- | Use a Stream as input to a resumable Attoparsec parser
--
-- >>> import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
-- >>> fst <$> parse (Attoparsec.decimal <* Attoparsec.endOfInput) (yield "123" *> yield "456")
-- Right 123456
parse ::
  forall a m r.
  Monad m =>
  Parser a ->
  Stream ByteString m r ->
  m (Either String a, Stream ByteString m r)
parse parser stream0 = do
  step <- next stream0
  case step of
    Left r -> case Attoparsec.parseOnly parser "" of
      Left message -> pure (Left message, pure r)
      Right a -> pure (Right a, pure r)
    Right (chunk, rest) -> loop (Attoparsec.parse parser chunk) rest
  where
    loop ::
      Result a ->
      Stream ByteString m r ->
      m (Either String a, Stream ByteString m r)
    loop result stream =
      case result of
        Fail remaining [] message -> pure (Left message, leftovers remaining stream)
        Fail remaining contexts message ->
          pure (Left (List.intercalate " > " contexts <> ": " <> message), leftovers remaining stream)
        Done remaining a -> pure (Right a, leftovers remaining stream)
        Partial cont -> do
          step <- next stream
          case step of
            Left r -> case Attoparsec.eitherResult (cont "") of
              Left message -> pure (Left message, pure r)
              Right a -> pure (Right a, pure r)
            Right (chunk, rest) -> loop (cont chunk) rest

    leftovers :: ByteString -> Stream ByteString m r -> Stream ByteString m r
    leftovers remaining stream = do
      yield remaining
      stream
