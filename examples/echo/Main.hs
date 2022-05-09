{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Maiwar (respond, status200)
import Maiwar.CLI (defaultMain)
import qualified Maiwar.Pipe as Pipe

main :: IO ()
main = defaultMain \_ ->
  respond status200 [] do
    Pipe.map id
