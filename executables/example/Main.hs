module Main where

import System.Environment (getArgs)

import Board
import Coord
import Piece (Kind(FU))
import Shogi
import Shogi.Const (initialShogi)

mainNumberOfBoards :: IO ()
mainNumberOfBoards = do
  args <- getArgs
  let num = read (head args) :: Int
  print $ length $ te num

te :: Int -> [Shogi]
te 0 = [initialShogi]
te n = concatMap getNext $ te $ n - 1

main :: IO ()
main =
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
