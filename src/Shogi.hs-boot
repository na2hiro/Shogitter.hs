{-# LANGUAGE GADTs #-}
module Shogi where

import Coord
import Piece
import Board
import Hands
import Data.Vector(Vector)

type Turn = Color

data Shogi m e a s mp where
    Shogi :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp) => Turn -> Board m e a s -> Hands -> Shogi m e a s mp
class MoverPredicator mp where
    canMove :: Shogi m e a s mp -> (Coord, Piece) -> Bool
    canMoveCoord :: Shogi m e a s mp -> Coord -> Bool
    canMoveCoord shogi@(Shogi _ board _) coord = canMove shogi (coord, unsafeGet coord board)

