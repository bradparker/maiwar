module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest
  [
    "-ilib",
    "-XImpredicativeTypes",
    "-XOverloadedLists",
    "-XOverloadedStrings",
    "lib"
  ]
