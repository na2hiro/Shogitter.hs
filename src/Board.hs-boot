{-# LANGUAGE GADTs #-}
module Board where

import Coord(Coord)
import Piece(Piece, Kind)
import Data.Array.IArray(Array)
import Color(Color)

type Cell = Maybe Piece

data Board a where
    Board :: (AbilityProxy a) => Array Coord Cell -> Board a

class AbilityProxy a where
    abilityProxy :: Color -> Coord -> Board a -> [Kind]

slice :: Coord -> Coord -> (Cell -> Bool) -> Board a -> [Cell]
unsafeGet :: Coord -> Board a -> Piece
get :: Coord -> Board a -> Cell
safeGet :: Coord -> Board a ->  Cell
addCoord :: Color -> Coord -> Coord -> Coord
