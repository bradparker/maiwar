{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative ((<|>))
import Maiwar (serve)
import qualified Maiwar.CLI
import Maiwar.Handlers.Static (static)
import Maiwar.Middleware.Logged (logged)
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    info,
    long,
    option,
    str,
  )
import System.IO (hSetBuffering, BufferMode (LineBuffering), stdout)

optionsParser :: Parser (FilePath, Maiwar.CLI.Options)
optionsParser =
  (,)
    <$> (option str (long "directory") <|> pure ".")
    <*> Maiwar.CLI.optionsParser

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (directory, options) <- execParser (info optionsParser fullDesc)
  config <- Maiwar.CLI.optionsToConfig options
  serve config (logged (static directory))
