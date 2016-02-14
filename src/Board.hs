{-# LANGUAGE GADTs #-}
module Board where

import Piece(Piece(..), Kind(..), Coord, Color(..), Promoted, move, MoveDef(..), uniqueMoveDef)
import Coord(Coord(..), getY, direct)
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Slicer(NormalSlicer)
import Data.Array.IArray as A(Array, listArray, (!), (//), bounds, indices, assocs)
import Data.List(transpose, nub)
import Data.Ix as Ix(inRange)
import Data.Maybe(maybeToList, isJust, fromJust)
import Control.Monad(guard)
import Control.Arrow(second)

type Cell = Maybe Piece
data Move = Move Coord Coord Promoted | Put Coord Kind deriving (Show, Eq)

data Board a s where
    Board :: (AbilityProxy a, Slicer s) => Array Coord Cell -> Board a s
instance Eq (Board a s) where
    Board arr == Board arr2 = arr == arr2
instance Show (Board a s) where
    show board = concat$ do
        y <- [1..9]
        return. (\line->"P"++show y++line++"\n"). concat$ do
            x <- [9,8..1]
            return. showCell$ get (Coord x y) board
        where showCell Nothing = " * "
              showCell (Just p) = show p

type NormalBoard = Board NormalAbilityProxy NormalSlicer

class AbilityProxy a where
    abilityProxy :: Color -> Coord -> Board a s -> [Kind]

class Slicer s where
    slice :: Coord -> Coord -> Board a s -> [Coord]

initialBoard :: (AbilityProxy a, Slicer s) => Board a s
initialBoard = Board initialArray

initialArray :: Array Coord Cell
initialArray = listArray (Coord 1 1, Coord 9 9)$ concat. transpose$ gote++replicate 3 four++sente
    where one = map Just $ [KY .. KI]++OU:[KI,GI .. KY]
          two = Nothing:Just KA:replicate 5 Nothing++[Just HI,Nothing]
          three = replicate 9 $ Just FU
          four = replicate 9 Nothing
          gote :: [[Cell]]
          gote =  map (map (fmap (Piece White False))) [one,two,three]
          sente :: [[Cell]]
          sente = reverse$ map (reverse . map (fmap (\(Piece color _ kind)->Piece Black False kind))) gote

unsafeGet :: Coord -> Board a s -> Piece
unsafeGet c b = let Just p = get c b in p

get :: Coord -> Board a s -> Cell
get c (Board b) = b!c

safeGet :: Coord -> Board a s ->  Cell
safeGet c b@(Board a) = if b `Board.inRange` c then a!c else Nothing

set :: (Coord, Cell) -> Board a s -> Board a s
set cp = sets [cp]

sets :: [(Coord, Cell)] -> Board a s -> Board a s
sets cps (Board b) = Board$ b // cps

inRange :: Board a s -> Coord -> Bool
inRange board = Ix.inRange$ Board.bounds board

bounds :: Board a s -> (Coord, Coord)
bounds (Board arr) = A.bounds arr

coords :: Board a s -> [Coord]
coords (Board arr) = indices arr

cells :: Board a s -> [(Coord, Cell)]
cells (Board arr) = assocs arr

pieces :: Board a s -> [(Coord, Piece)]
pieces = catSndMaybes. cells

catSndMaybes :: [(a, Maybe b)] -> [(a, b)]
catSndMaybes = map (second fromJust). filter (isJust. snd)

destinationsAt :: Coord -> Board a s -> [Coord]
destinationsAt from board@(Board _) = nub$ do
    Piece color promoted kind <- maybeToList$ get from board
    moveDef <- uniqueMoveDef$ do
        ability <- abilityProxy color from board
        move ability promoted
    case moveDef of
        Exact vec -> take 1$ dests color from (direct color vec)
        Slide vec -> dests color from (direct color vec)
    where dests :: Color -> Coord -> Coord -> [Coord]
          dests color from vec = takeW color$ map (\coord->(coord, get coord board))$ slice from vec board
          takeW :: Color -> [(Coord, Cell)] -> [Coord]
          takeW _ [] = []
          takeW c ((to, Just (Piece c' _ _)):_) | c==c' = []
                                                | otherwise = [to]
          takeW c ((to,_):xs) = to:takeW c xs

addCoord :: Color -> Coord -> Coord -> Coord
addCoord Black v x = x+v
addCoord White v x = x-v

canPromote :: Color -> Board a s -> Coord -> Bool
canPromote Black _ c = getY c<=3
canPromote _ board coord = canPromote Black board$ reverseCoord board coord

reverseCoord :: Board a s -> Coord -> Coord
reverseCoord board c = max+min-c
    where (min, max) = Board.bounds board
