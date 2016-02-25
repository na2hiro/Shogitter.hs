{-# LANGUAGE GADTs #-}
module Shogi where

import Coord
import Piece
import Board
import Hands
import Data.Vector(Vector)

type Turn = Color

data Shogi a s mp where
    Shogi :: (AbilityProxy a, Slicer s, MoverPredicator mp) => Turn -> Board a s -> Hands -> Shogi a s mp
class MoverPredicator mp where
    canMove :: Shogi a s mp -> (Coord, Piece) -> Bool
    canMoveCoord :: Shogi a s mp -> Coord -> Bool
    canMoveCoord shogi@(Shogi _ board _) coord = canMove shogi (coord, unsafeGet coord board)

