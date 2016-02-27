{-# LANGUAGE GADTs #-}
module Board where

import Coord(Coord)
import Piece(Piece, Kind, Promoted)
import Data.Vector as V(Vector)
import Color(Color)

type Cell = Maybe Piece
data Move = Move Coord Coord Promoted | Put Coord Kind

data Board m a s where
    Board :: (Mover m, AbilityProxy a, Slicer s) => (Int, Int) -> Vector Cell -> Board m a s

class AbilityProxy a where
    abilityProxy :: Color -> Coord -> Board m a s -> [(Promoted, Kind)]
class Slicer s where
    sliceAsCoord :: Board m a s -> Coord -> Coord -> [Coord]
    slice :: Board m a s -> Coord -> Coord -> [(Coord, Cell)]
    slice board base vec = map (\coord -> (coord, get coord board))$ sliceAsCoord board base vec
    regularity :: Board m a s -> Bool
class Mover m where
    move :: Move -> Board m a s -> (Board m a s, [Kind])

unsafeGet :: Coord -> Board m a s -> Piece
get :: Coord -> Board m a s -> Cell
safeGet :: Coord -> Board m a s ->  Cell
sets :: [(Coord, Cell)] -> Board m a s -> Board m a s
addCoord :: Color -> Coord -> Coord -> Coord
inRange :: Board m a s -> Coord -> Bool
bounds :: Board m a s -> (Coord, Coord)
