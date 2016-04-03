{-# LANGUAGE GADTs #-}
module Board where

import Coord(Coord)
import Piece(Piece, Kind, Promoted)
import Data.Vector as V(Vector)
import Color(Color)

type Cell = Maybe Piece
data Move = Move Coord Coord Promoted | Put Coord Kind

data Board m e a s mp where
    Board :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp) =>
        (Int, Int) -> Vector Cell -> Board m e a s mp

class AbilityProxy a where
    abilityProxy :: Color -> Coord -> Board m e a s mp -> [(Promoted, Kind)]
class Slicer s where
    sliceAsCoord :: Board m e a s mp -> Coord -> Coord -> [Coord]
    slice :: Board m e a s mp -> Coord -> Coord -> [(Coord, Cell)]
    slice board base vec = map (\coord -> (coord, get coord board))$ sliceAsCoord board base vec
    sliceFinite :: Board m e a s mp -> Coord -> Coord -> [(Coord, Cell)]
    sliceFinite board base vec = map (\coord -> (coord, get board coord))$ sliceAsCoordFinite board base vec
    sliceAsCoordFinite :: Board m e a s mp -> Coord -> Coord -> [Coord]
    sliceAsCoordFinite board base vec = if regularity board || null slices
        then slices
        else f slices
        where slices = sliceAsCoord board base vec
              first = head slices
              f [x] = [x]
              f (x:y:xs) | base==x && first == y = [x]
              f (_:y:xs) = f (y:xs)
    regularity :: Board m e a s mp -> Bool
class Mover m where
    move :: Color -> Move -> Board m e a s mp -> (Board m e a s mp, [Kind])
class Effector e where
    effect :: Coord -> Coord -> Board m e a s mp -> Board m e a s mp
    effectPut :: Coord -> Board m e a s mp -> Board m e a s mp
    effectPut = effect (error "Define `effectPut` for Effector if `effect` uses `from` parameter")
class MoverPredicator mp where
    canMove :: Board m e a s mp -> (Coord, Piece) -> Bool
    canMoveCoord :: Board m e a s mp -> Coord -> Bool
    canMoveCoord board coord = canMove board (coord, unsafeGet board coord)

unsafeGet :: Board m e a s mp -> Coord -> Piece
get :: Board m e a s mp -> Coord -> Cell
safeGet :: Board m e a s mp -> Coord -> Cell
sets :: Board m e a s mp -> [(Coord, Cell)] -> Board m e a s mp
set :: Board m e a s mp -> (Coord, Cell) -> Board m e a s mp
addCoord :: Color -> Coord -> Coord -> Coord
inRange :: Board m e a s mp -> Coord -> Bool
bounds :: Board m e a s mp -> (Coord, Coord)
destinationsAt :: Board m e a s mp -> Coord -> [Coord]
cells :: Board m e a s mp -> [(Coord, Cell)]
