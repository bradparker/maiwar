module Main where

import Control.Monad (replicateM_)

stream :: IO ()
stream = replicateM_ 1000 (putStrLn "Hey!")

main :: IO ()
main = stream
