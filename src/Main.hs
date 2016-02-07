module Main where

import Shogi
import Board
import Piece(Kind(FU))

main :: IO ()
main = putStrLn$ concatMap show [te0, te1, te2, te3, te4]
    where te0 = initialShogi
          te1 = unsafeDoMove (Move (7,7) (7,1) True) te0
          te2 = unsafeDoMove (Move (6,1) (7,1) False) te1
          te3 = unsafeDoMove (Move (8,8) (7,1) True) te2
          te4 = unsafeDoMove (Put (5,8) FU) te3
