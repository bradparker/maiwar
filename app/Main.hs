{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative ((<|>))
import Maiwar (serve)
import qualified Maiwar.CLI
import Maiwar.Handlers.Static (static)
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    info,
    long,
    option,
    str,
  )

optionsParser :: Parser (FilePath, Maiwar.CLI.Options)
optionsParser =
  (,)
    <$> (option str (long "directory") <|> pure ".")
    <*> Maiwar.CLI.optionsParser

main :: IO ()
main = do
  (directory, options) <- execParser (info optionsParser fullDesc)
  config <- Maiwar.CLI.optionsToConfig options
  serve config (static directory)
