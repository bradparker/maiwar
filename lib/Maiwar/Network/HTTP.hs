{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Maiwar.Network.HTTP where

import Control.Applicative (empty)
import Control.Monad (join, when, (<=<))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import GHC.Exts (IsList (fromList, toList, type Item), IsString (fromString))
import Maiwar.Pipe (Consumer, Pipe, receive, send, subInput, (>->))
import qualified Maiwar.Pipe as Pipe
import qualified Maiwar.Pipe.Attoparsec.ByteString as Pipe.Attoparsec
import qualified Maiwar.Pipe.System.Timeout as Pipe
import Maiwar.Stream (Stream, flush)
import qualified Maiwar.Stream.Attoparsec.ByteString as Stream.Attoparsec
import qualified Maiwar.Stream.ByteString as Stream.ByteString
import Numeric (showHex)
import Text.Read (readMaybe)

newtype Method = Method ByteString
  deriving (Show)

newtype RequestTarget = RequestTarget ByteString
  deriving (Show, Eq, Ord)

data HTTPVersion = HTTPVersion Int Int
  deriving (Show)

http11 :: HTTPVersion
http11 = HTTPVersion 1 1

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

status301 :: Status
status301 = Status 301 "Moved Permanently"

status400 :: Status
status400 = Status 400 "Bad Request"

status404 :: Status
status404 = Status 404 "Not Found"

status405 :: Status
status405 = Status 405 "Method Not Allowed"

data Response input output m r = Response
  { status :: Status,
    headers :: Headers,
    body :: Pipe input output m r
  }

response400 :: Monad m => Response input output m ()
response400 = Response status400 [] (pure ())

crlfParser :: Parser ByteString
crlfParser = "\r\n"

-- | Request parser
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
    <$> Attoparsec.manyTill'
      Attoparsec.anyChar
      ( crlfParser *> do
          nextByte <- Attoparsec.peekChar
          case nextByte of
            Nothing -> pure ()
            Just b -> when (Attoparsec.inClass " \t" b) empty
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
headersParser = Headers <$> Attoparsec.manyTill' headerFieldParser crlfParser

chunkSizeParser :: Parser Int
chunkSizeParser = Attoparsec.hexadecimal <* crlfParser

requestBody ::
  forall m a.
  Monad m =>
  Headers ->
  Stream ByteString m a ->
  Stream ByteString m (Stream ByteString m a)
requestBody headers stream =
  if isChunked headers
    then chunkedBody stream
    else case contentLength headers of
      Nothing -> pure stream
      Just i -> knownBody i stream

knownBody ::
  forall m a.
  Monad m =>
  Int ->
  Stream ByteString m a ->
  Stream ByteString m (Stream ByteString m a)
knownBody = Stream.ByteString.splitAt

chunkedBody ::
  forall m a.
  Monad m =>
  Stream ByteString m a ->
  Stream ByteString m (Stream ByteString m a)
chunkedBody stream = do
  (result, rest) <- lift (Stream.Attoparsec.parse chunkSizeParser stream)
  case result of
    Left _e -> pure rest
    Right size -> do
      if size == 0
        then pure (Stream.ByteString.drop 2 rest)
        else chunkedBody . Stream.ByteString.drop 2 =<< Stream.ByteString.splitAt size rest

-- | Chunk-encode stream of ByteStrings
--
-- >>> import Maiwar.Pipe (evalPipe, (>-))
-- >>> import qualified Maiwar.Pipe as Pipe
-- >>> import qualified Maiwar.Stream as Stream
-- >>> Stream.run (evalPipe (encodeChunks >- Pipe.print) (Stream.yield "Hey" *> Stream.yield "There"))
-- "3\r\nHey\r\n"
-- "5\r\nThere\r\n"
-- "0\r\n\r\n"
encodeChunks :: forall m. Monad m => Pipe ByteString ByteString m ()
encodeChunks =
  Pipe.filter (not . BSC.null) >-> do
    Pipe.map encodeChunk
    send finalChunk
  where
    finalChunk :: ByteString
    finalChunk = "0\r\n\r\n"

encodeChunk :: ByteString -> ByteString
encodeChunk bytes = BSC.pack (showHex (BSC.length bytes) "\r\n") <> bytes <> "\r\n"

serializeResponsePreamble :: HTTPVersion -> Status -> Headers -> ByteString
serializeResponsePreamble httpVersion status headers =
  serializeVersion httpVersion
    <> " "
    <> serializeStatus status
    <> "\r\n"
    <> serializeHeaders headers
    <> "\r\n"

serializeVersion :: HTTPVersion -> ByteString
serializeVersion (HTTPVersion major minor) = BSC.pack ("HTTP/" <> show major <> "." <> show minor)

serializeStatus :: Status -> ByteString
serializeStatus (Status code message) = BSC.pack (show code) <> " " <> message

serializeHeaders :: Headers -> ByteString
serializeHeaders = foldMap serializeHeader . toList

serializeHeader :: HeaderField -> ByteString
serializeHeader (HeaderField (HeaderFieldName name) content) = name <> ": " <> content <> "\r\n"

-- | Convert a Response into a Pipe of ByteStrings
-- Ready to receive from and send to a Socket or Context.
--
-- >>> import Maiwar.Pipe (evalPipe, (>-))
-- >>> import qualified Maiwar.Pipe as Pipe
-- >>> import qualified Maiwar.Stream as Stream
-- >>> let emptyResponse = Response status200 [] (pure ())
-- >>> Stream.run (evalPipe (sendResponseChunked emptyResponse >- Pipe.print) (Stream.yield "Hey" *> Stream.yield "There"))
-- "HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n"
-- >>> let nonEmptyResponse = Response status200 [] (Pipe.send "Hey" *> Pipe.send "There")
-- >>> Stream.run (evalPipe (sendResponseChunked nonEmptyResponse >- Pipe.print) (Stream.yield "Hey" *> Stream.yield "There"))
-- "HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n"
-- "3\r\nHey\r\n"
-- "5\r\nThere\r\n"
-- "0\r\n\r\n"
sendResponseChunked ::
  forall m.
  Monad m =>
  Response ByteString ByteString m () ->
  Pipe ByteString ByteString m ()
sendResponseChunked response =
  response.body >-> do
    input <- receive
    case input of
      Nothing -> do
        send
          ( serializeResponsePreamble
              http11
              response.status
              (response.headers <> ["Content-Length" =: "0"])
          )
      Just bytes -> do
        send
          ( serializeResponsePreamble
              http11
              response.status
              (addChunkedEncoding response.headers)
          )
        send (encodeChunk bytes)
        encodeChunks
  where
    addChunkedEncoding :: Headers -> Headers
    addChunkedEncoding = alterHeader "Transfer-Encoding" \case
      Nothing -> Just "chunked"
      Just existing -> Just (existing <> ", chunked")

sendResponseBuffered ::
  forall m.
  Monad m =>
  Response ByteString ByteString m () ->
  Pipe ByteString ByteString m ()
sendResponseBuffered response =
  response.body >-> do
    body <- Pipe.fold
    send
      ( serializeResponsePreamble
          http11
          response.status
          ( response.headers
              <> [ "Content-Length" =: BSC.pack (show (BSC.length body))
                 ]
          )
      )
    send body

sendResponse ::
  forall m.
  Monad m =>
  HTTPVersion ->
  Response ByteString ByteString m () ->
  Pipe ByteString ByteString m ()
sendResponse version =
  case version of
    HTTPVersion 1 0 -> sendResponseBuffered
    _ -> sendResponseChunked

type Handler m result =
  Request -> Consumer ByteString m (Response ByteString ByteString m result)

handleRequest ::
  Monad m =>
  Handler m () ->
  Request ->
  Pipe ByteString ByteString m ()
handleRequest handler request =
  subInput (requestBody request.headers) (join . flush) do
    when (findHeader "Expect" request.headers == Just "100-continue") do
      send "HTTP/1.1 100 Continue\r\n\r\n"
    sendResponse request.httpVersion =<< handler request

handleConnection ::
  forall m.
  (MonadIO m, MonadThrow m) =>
  Handler m () ->
  Pipe ByteString ByteString m ()
handleConnection handler = go
  where
    go = do
      result <- Pipe.timeout 3000000 (Pipe.Attoparsec.parse requestParser)
      case result of
        Left _e -> sendResponse (HTTPVersion 1 1) response400
        Right request -> do
          Pipe.timeout 300000000 (handleRequest handler request)
          go
