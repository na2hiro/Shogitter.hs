{-# LANGUAGE GADTs #-}
module Board where

import Coord(Coord)
import Piece(Piece, Kind, Promoted)
import Data.Array.IArray(Array)
import Color(Color)

type Cell = Maybe Piece

data Board a s where
    Board :: (AbilityProxy a, Slicer s) => Array Coord Cell -> Board a s

class AbilityProxy a where
    abilityProxy :: Color -> Coord -> Board a s -> [(Promoted, Kind)]
class Slicer s where
    slice :: Coord -> Coord -> Board a s -> [Coord]
    regularity :: Board a s -> Bool

unsafeGet :: Coord -> Board a s -> Piece
get :: Coord -> Board a s -> Cell
safeGet :: Coord -> Board a s ->  Cell
addCoord :: Color -> Coord -> Coord -> Coord
inRange :: Board a s -> Coord -> Bool
bounds :: Board a s -> (Coord, Coord)
