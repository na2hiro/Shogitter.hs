{-# LANGUAGE GADTs #-}
module Board where

import Coord(Coord)
import Piece(Piece, Kind, Promoted)
import Data.Vector as V(Vector)
import Color(Color)

type Cell = Maybe Piece
data Move = Move Coord Coord Promoted | Put Coord Kind

data Board m e a s where
    Board :: (Mover m, Effector e, AbilityProxy a, Slicer s) => (Int, Int) -> Vector Cell -> Board m e a s

class AbilityProxy a where
    abilityProxy :: Color -> Coord -> Board m e a s -> [(Promoted, Kind)]
class Slicer s where
    sliceAsCoord :: Board m e a s -> Coord -> Coord -> [Coord]
    slice :: Board m e a s -> Coord -> Coord -> [(Coord, Cell)]
    slice board base vec = map (\coord -> (coord, get coord board))$ sliceAsCoord board base vec
    regularity :: Board m e a s -> Bool
class Mover m where
    move :: Move -> Board m e a s -> (Board m e a s, [Kind])
class Effector e where
    effect :: Coord -> Board m e a s -> Board m e a s
    effectPut :: Coord -> Board m e a s -> Board m e a s
    effectPut = effect

unsafeGet :: Coord -> Board m e a s -> Piece
get :: Coord -> Board m e a s -> Cell
safeGet :: Coord -> Board m e a s ->  Cell
sets :: [(Coord, Cell)] -> Board m e a s -> Board m e a s
addCoord :: Color -> Coord -> Coord -> Coord
inRange :: Board m e a s -> Coord -> Bool
bounds :: Board m e a s -> (Coord, Coord)
