module Main where

import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Tree.Game_tree.Game_tree (Game_tree(..))
import System.Environment (getArgs)

import Board
import Coord
import Piece (Kind(FU))
import Search.MaterialAlphaBeta
import Server.Server (serve)
import Shogi
import Shogi.Const (initialShogi)

main :: IO ()
main = do
  args <- getArgs
  let input = pack (args !! 0)
  let output = serve input
  putStrLn $ unpack output

-- | Alpha beta search
main3 :: IO ()
main3 = do
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

mainNumberOfBoards :: IO ()
mainNumberOfBoards = do
  args <- getArgs
  let num = read (head args) :: Int
  print $ length $ te num

te :: Int -> [Shogi]
te 0 = [initialShogi]
te n = concatMap getNext $ te $ n - 1

main2 :: IO ()
main2 =
  putStrLn $
  concatMap show [te0, te1, te2, te3, te4] ++
  show (getMovesShogi te0) ++ "te1s" ++ show te1s ++ "te2s" ++ show te2s
  where
    te0 = initialShogi :: Shogi
    te1 = unsafeDoMove (Move (Coord 7 7) (Coord 7 1) True) te0
    te2 = unsafeDoMove (Move (Coord 6 1) (Coord 7 1) False) te1
    te3 = unsafeDoMove (Move (Coord 8 8) (Coord 7 1) True) te2
    te4 = unsafeDoMove (Put (Coord 5 8) FU) te3
    te1s = getNext te0
    te2s = concatMap getNext te1s
