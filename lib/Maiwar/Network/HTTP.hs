{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Network.HTTP where

import Control.Monad ((<=<))
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import GHC.Exts (IsList (fromList, toList, type Item), IsString (fromString))
import Text.Read (readMaybe)

newtype Method = Method ByteString
  deriving (Show)

newtype RequestTarget = RequestTarget ByteString
  deriving (Show)

data HTTPVersion = HTTPVersion Int Int
  deriving (Show)

newtype Headers = Headers [HeaderField]
  deriving (Show, Semigroup)

instance IsList Headers where
  type Item Headers = HeaderField

  fromList :: [HeaderField] -> Headers
  fromList = Headers

  toList :: Headers -> [HeaderField]
  toList (Headers fields) = fields

deleteHeader :: HeaderFieldName -> Headers -> Headers
deleteHeader name = fromList . List.filter ((name /=) . (.name)) . toList

insertHeader :: HeaderField -> Headers -> Headers
insertHeader field = fromList . (field :) . toList . deleteHeader field.name

alterHeader :: HeaderFieldName -> (Maybe ByteString -> Maybe ByteString) -> Headers -> Headers
alterHeader name f headers =
  case f (findHeader name headers) of
    Nothing -> deleteHeader name headers
    Just value -> insertHeader (HeaderField name value) headers

findHeader :: HeaderFieldName -> Headers -> Maybe ByteString
findHeader name = fmap (.value) . List.find ((== name) . (.name)) . toList

contentLength :: Headers -> Maybe Int
contentLength = readMaybe <=< fmap BSC.unpack . findHeader "Content-Length"

transferEncoding :: Headers -> Maybe [ByteString]
transferEncoding = fmap (fmap BSC.strip . BSC.split ',') . findHeader "Transfer-Encoding"

isChunked :: Headers -> Bool
isChunked = (Just "chunked" ==) . (listToMaybe . reverse <=< transferEncoding)

newtype HeaderFieldName = HeaderFieldName ByteString
  deriving (Show, Eq)

headerFieldName :: ByteString -> HeaderFieldName
headerFieldName = HeaderFieldName . BSC.map Char.toLower

instance IsString HeaderFieldName where
  fromString = headerFieldName . BSC.pack

data HeaderField = HeaderField
  { name :: HeaderFieldName,
    value :: ByteString
  }
  deriving (Show)

(=:) :: HeaderFieldName -> ByteString -> HeaderField
(=:) = HeaderField

data Request = Request
  { method :: Method,
    target :: RequestTarget,
    httpVersion :: HTTPVersion,
    headers :: Headers
  }
  deriving (Show)

data Status = Status Int ByteString
  deriving (Eq)

status200 :: Status
status200 = Status 200 "OK"

status400 :: Status
status400 = Status 400 "Bad request"

status404 :: Status
status404 = Status 404 "Not found"

data Response body = Response
  { httpVersion :: HTTPVersion,
    status :: Status,
    headers :: Headers,
    body :: body
  }

crlfParser :: Parser ByteString
crlfParser = "\r\n"

-- | Request parser
--
-- >>> Attoparsec.parseOnly requestParser "GET /posts HTTP/1.1\r\n\r\n"
-- Right (Request {method = Method "GET", target = RequestTarget "/posts", httpVersion = HTTPVersion 1 1, headers = Headers []})
-- >>> Attoparsec.parseOnly requestParser "GET /posts/getting-close-to-the-conceptual-metal HTTP/1.0\r\nAccept: text/html\r\n\r\n"
-- Right (Request {method = Method "GET", target = RequestTarget "/posts/getting-close-to-the-conceptual-metal", httpVersion = HTTPVersion 1 0, headers = Headers [HeaderField {name = HeaderFieldName "accept", value = "text/html"}]})
-- >>> Attoparsec.parseOnly requestParser "GET /hello.txt HTTP/1.1\r\nUser-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\nHost: www.example.com\r\nAccept-Language: en, mi\r\n\r\n"
-- Right (Request {method = Method "GET", target = RequestTarget "/hello.txt", httpVersion = HTTPVersion 1 1, headers = Headers [HeaderField {name = HeaderFieldName "user-agent", value = "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3"},HeaderField {name = HeaderFieldName "host", value = "www.example.com"},HeaderField {name = HeaderFieldName "accept-language", value = "en, mi"}]})
-- >>> Attoparsec.eitherResult (Attoparsec.feed (Attoparsec.parse requestParser "POST /foobar HTTP/1.1\r\nHost: localhost:5000\r\nUser-Agent: curl/7.74.0\r\nAccept: */*\r\nContent-Length: 2\r") "\nContent-Type: application/x-www-form-urlencoded\r\n\r\n")
-- Right (Request {method = Method "POST", target = RequestTarget "/foobar", httpVersion = HTTPVersion 1 1, headers = Headers [HeaderField {name = HeaderFieldName "host", value = "localhost:5000"},HeaderField {name = HeaderFieldName "user-agent", value = "curl/7.74.0"},HeaderField {name = HeaderFieldName "accept", value = "*/*"},HeaderField {name = HeaderFieldName "content-length", value = "2"},HeaderField {name = HeaderFieldName "content-type", value = "application/x-www-form-urlencoded"}]})
-- >>> Attoparsec.parseOnly requestParser "GET /hello HTTP/1.1\r\nHost: localhost:8080\r\nUser-Agent: curl/7.74.0\r\nAccept: */*\r\n\r\n"
-- Right (Request {method = Method "GET", target = RequestTarget "/hello", httpVersion = HTTPVersion 1 1, headers = Headers [HeaderField {name = HeaderFieldName "host", value = "localhost:8080"},HeaderField {name = HeaderFieldName "user-agent", value = "curl/7.74.0"},HeaderField {name = HeaderFieldName "accept", value = "*/*"}]})
requestParser :: Parser Request
requestParser =
  Request
    <$> (Method <$> Attoparsec.takeWhile (/= ' ') <* " ")
    <*> (RequestTarget <$> Attoparsec.takeWhile (/= ' ') <* " ")
    <*> (HTTPVersion <$> ("HTTP/" *> digitParser) <*> (Attoparsec.char '.' *> digitParser))
    <* crlfParser
    <*> headersParser
  where
    digitParser :: Parser Int
    digitParser = Char.digitToInt <$> Attoparsec.digit

-- | Header field name parser
--
-- >>> Attoparsec.parseOnly headerFieldNameParser "Content-type:"
-- Right (HeaderFieldName "content-type")
headerFieldNameParser :: Parser HeaderFieldName
headerFieldNameParser = headerFieldName <$> Attoparsec.takeWhile (/= ':') <* ":"

-- | Header field content parser
--
-- >>> Attoparsec.parseOnly headerFieldContentParser "Foo\r\n\tBar\r\n  Baz\r\n"
-- Right "Foo\r\n\tBar\r\n  Baz"
headerFieldContentParser :: Parser ByteString
headerFieldContentParser =
  BSC.pack
    <$> Attoparsec.manyTill
      Attoparsec.anyChar
      ( crlfParser *> do
          nextByte <- Attoparsec.peekChar
          case nextByte of
            Nothing -> pure ()
            Just b ->
              if Attoparsec.inClass " \t" b
                then fail "whitespace continue"
                else pure ()
      )

-- | Header field parser
--
-- >>> Attoparsec.parseOnly headerFieldParser "Content-type: application/json\r\n"
-- Right (HeaderField {name = HeaderFieldName "content-type", value = "application/json"})
headerFieldParser :: Parser HeaderField
headerFieldParser =
  HeaderField
    <$> headerFieldNameParser
    <*> (Attoparsec.takeWhile (Attoparsec.inClass " \t") *> headerFieldContentParser)

-- | Headers parser
--
-- >>> Attoparsec.parseOnly headersParser "Host: example.com\r\nContent-type: application/json\r\n\r\n"
-- Right (Headers [HeaderField {name = HeaderFieldName "host", value = "example.com"},HeaderField {name = HeaderFieldName "content-type", value = "application/json"}])
headersParser :: Parser Headers
headersParser = Headers <$> Attoparsec.manyTill headerFieldParser crlfParser

chunkSizeParser :: Parser Int
chunkSizeParser = Attoparsec.hexadecimal <* crlfParser
