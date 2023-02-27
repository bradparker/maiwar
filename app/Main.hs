{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative (many, (<|>))
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BSC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Maiwar (serve)
import qualified Maiwar.CLI
import Maiwar.Handler (RequestTarget (RequestTarget))
import Maiwar.Handlers.Static (static)
import Maiwar.Middleware.Logged (logged)
import Maiwar.Middleware.Redirecting (redirecting)
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    info,
    long,
    option,
    str,
  )
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

data Options = Options
  { directory :: FilePath,
    redirects :: Map RequestTarget RequestTarget,
    baseOptions :: Maiwar.CLI.Options
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> (option str (long "directory") <|> pure ".")
    <*> redirectsParser
    <*> Maiwar.CLI.optionsParser
  where
    redirectsParser :: Parser (Map RequestTarget RequestTarget)
    redirectsParser = Map.fromList . map parseRedirect <$> many (option str (long "redirect"))

    parseRedirect :: String -> (RequestTarget, RequestTarget)
    parseRedirect = bimap (RequestTarget . BSC.pack) (RequestTarget . BSC.pack . drop 1) . break (== ':')

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  options <- execParser (info optionsParser fullDesc)
  config <- Maiwar.CLI.optionsToConfig options.baseOptions
  serve config (logged (redirecting options.redirects (static options.directory)))
