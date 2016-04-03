{-# LANGUAGE GADTs #-}
module Shogi where

import Coord
import Piece
import Board
import Hands
import Data.Vector(Vector)

type Turn = Color

data Result = Win Color
            | Even

data Shogi m e a s mp j where
    Shogi :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp, Judge j) => Turn -> Board m e a s mp -> Hands -> Shogi m e a s mp j

class Judge j where
    judge :: Shogi m e a s mp j -> Maybe Result

getNextWithoutJudge :: Shogi m e a s mp j -> [Shogi m e a s mp j]
