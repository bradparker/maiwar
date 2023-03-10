module Main where

import Control.Monad (replicateM_)
import Maiwar.Stream (Stream)
import qualified Maiwar.Stream as Stream
import Data.Function ((&))

stream :: Stream String IO ()
stream = replicateM_ 1000 (Stream.yield "Hey!")

main :: IO ()
main = stream
  & Stream.traverse putStrLn
  & Stream.run
