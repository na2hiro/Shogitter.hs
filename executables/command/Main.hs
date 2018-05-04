module Main where

import Data.ByteString.Lazy.Char8 (pack, unpack)
import System.Environment (getArgs)

import Server.Server (serve)

main :: IO ()
main = do
  args <- getArgs
  let input = pack (args !! 0)
  let output = serve input
  putStrLn $ unpack output
