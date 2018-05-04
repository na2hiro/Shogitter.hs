module Main where

import Data.Tree.Game_tree.Game_tree (Game_tree(..))
import System.Environment (getArgs)

import Search.MaterialAlphaBeta
import Shogi.Const (initialShogi)

-- | Alpha beta search
main :: IO ()
main = do
  args <- getArgs
  let num = read (args !! 0) :: Int
  let cached = read (args !! 1) :: Bool
  let shogi = initialShogi
  if cached
    then printAlphaBeta num (cachify shogi)
    else printAlphaBeta num shogi

printIterativeDeepeningAlphaBeta ::
     (Game_tree game, Show game) => Int -> game -> IO ()
printIterativeDeepeningAlphaBeta n shogi =
  print $ zip [1 .. n] (iterativeDeepeningAlphaBeta shogi)

printAlphaBeta :: (Game_tree game, Show game) => Int -> game -> IO ()
printAlphaBeta n shogi = print $ alphaBeta shogi n

