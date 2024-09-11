{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Maiwar.MSF.Attoparsec.ByteString where

import Control.Arrow (arr, loop)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Fix (MonadFix)
import Data.Attoparsec.ByteString (IResult (Done, Fail, Partial), Parser)
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.List as List
import Maiwar.MSF (MSF (MSF), feedback)

type Result a = (ByteString, Either String a)

renderError :: [String] -> String -> String
renderError [] message = message
renderError contexts message = List.intercalate " > " contexts <> ": " <> message

-- | Parse inputs using an Attoparsec Parser TODO: Make this much tidier
-- >>> import Maiwar.MSF (embed)
-- >>> embed (parse (Attoparsec.string "abcd")) ["abcd"]
-- [Just ("",Right "abcd")]
-- >>> embed (parse (Attoparsec.string "abcd")) ["abc", "def", "ghi", "jkl"]
-- [Nothing,Just ("ef",Right "abcd"),Nothing,Nothing]
-- >>> embed (parse (Attoparsec.string "abcd")) ["abc", "efg", "hij", "klm"]
-- [Nothing,Just ("abcefg",Left "string"),Nothing,Nothing]
-- >>> embed (parse (Attoparsec.string "START" *> Attoparsec.manyTill (Attoparsec.string "abcd") (Attoparsec.string "END"))) ["STA", "RTa", "bcd", "abc", "dabc", "dab", "cda", "bcd", "END"]
-- [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just ("",Right ["abcd","abcd","abcd","abcd","abcd"])]
parse :: forall a m. (MonadFix m) => Parser a -> MSF m ByteString (Maybe (Result a))
parse parser = MSF \bytes0 -> do
  case Attoparsec.parse parser bytes0 of
    Fail remaining contexts message ->
      pure (Just (remaining, Left (renderError contexts message)), pure Nothing)
    Done remaining a ->
      pure (Just (remaining, Right a), pure Nothing)
    Partial cont ->
      pure
        ( Nothing,
          feedback
            (Just (Partial cont))
            ( arr \(bytes, result) ->
                case result of
                  Nothing ->
                    (Nothing, Nothing)
                  Just (Fail remaining contexts message) ->
                    (Just (remaining, Left (renderError contexts message)), Nothing)
                  Just (Done remaining a) ->
                    (Just (remaining, Right a), Nothing)
                  Just (Partial cont) ->
                    case cont bytes of
                      Fail remaining contexts message ->
                        (Just (remaining, Left (renderError contexts message)), Nothing)
                      Done remaining a ->
                        (Just (remaining, Right a), Nothing)
                      Partial cont' ->
                        (Nothing, Just (Partial cont'))
            )
        )
