module Main where

import Data.ByteString.Lazy.Char8 (pack, unpack)
import System.Environment (getArgs)

import Server.Server (serveInitialBoard, serveMove)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["initialBoard", json] -> putStrLn $ unpack (serveInitialBoard (pack json))
    ["move", json]         -> putStrLn $ unpack (serveMove (pack json))
    _                      -> error $ "args length does not equal 2. args: : " ++ ( show args )
