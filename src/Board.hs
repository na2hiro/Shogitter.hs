{-# LANGUAGE GADTs #-}
module Board where

import Piece(Piece(..), Kind(..), Coord, Color(..), Promoted, move, MoveDef(..), uniqueMoveDef)
import Coord(Coord(..), getY, direct)
import Board.AbilityProxy(NormalAbilityProxy)
import Data.Array.IArray(Array, listArray, (!), (//), bounds, indices, assocs)
import Data.List(transpose)
import Data.Ix as Ix(inRange)
import Data.Maybe(maybeToList, isJust, fromJust)
import Control.Monad(guard)
import Control.Arrow(second)

type Cell = Maybe Piece
data Move = Move Coord Coord Promoted | Put Coord Kind deriving (Show, Eq)

data Board a where
    Board :: (AbilityProxy a) => Array Coord Cell -> Board a
instance Eq (Board a) where
    Board arr == Board arr2 = arr == arr2
instance Show (Board a) where
    show board = concat$ do
        y <- [1..9]
        return. (\line->"P"++show y++line++"\n"). concat$ do
            x <- [9,8..1]
            return. showCell$ get (Coord x y) board
        where showCell Nothing = " * "
              showCell (Just p) = show p

type NormalBoard = Board NormalAbilityProxy

class AbilityProxy a where
    abilityProxy :: Color -> Coord -> Board a -> [Kind]

initialBoard :: AbilityProxy a => Board a
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

unsafeGet :: Coord -> Board a -> Piece
unsafeGet c b = let Just p = get c b in p

get :: Coord -> Board a -> Cell
get c (Board b) = b!c

safeGet :: Coord -> Board a ->  Cell
safeGet c b@(Board a) = if b `Board.inRange` c then a!c else Nothing

set :: (Coord, Cell) -> Board a -> Board a
set cp = sets [cp]

sets :: [(Coord, Cell)] -> Board a -> Board a
sets cps (Board b) = Board$ b // cps

inRange :: Board a -> Coord -> Bool
inRange (Board arr) = Ix.inRange (bounds arr)

coords :: Board a -> [Coord]
coords (Board arr) = indices arr

cells :: Board a -> [(Coord, Cell)]
cells (Board arr) = assocs arr

pieces :: Board a -> [(Coord, Piece)]
pieces = catSndMaybes. cells

catSndMaybes :: [(a, Maybe b)] -> [(a, b)]
catSndMaybes = map (second fromJust). filter (isJust. snd)

slice :: Coord -> Coord -> (Cell -> Bool) -> Board a -> [Cell]
slice base vec cond board = if board `Board.inRange` next && cond cell
    then cell: slice next vec cond board
    else []
    where next = base + vec
          cell = get next board

destinationsAt :: Coord -> Board a -> [Coord]
destinationsAt from board@(Board _) = do
    Piece color promoted kind <- maybeToList$ get from board
    moveDef <- uniqueMoveDef$ do
        ability <- abilityProxy color from board
        move ability promoted
    case moveDef of
        Exact vec -> take 1$ dests color from (direct color vec)
        Slide vec -> dests color from (direct color vec)
    where dests :: Color -> Coord -> Coord -> [Coord]
          dests color from vec = do
             let to = from + vec
             guard$ board `Board.inRange` to
             case get to board of
                 Just (Piece color' _ _) | color==color' -> []
                                         | otherwise -> return to
                 _ -> to : dests color to vec

addCoord :: Color -> Coord -> Coord -> Coord
addCoord Black v x = x+v
addCoord White v x = x-v

canPromote :: Color -> Board a -> Coord -> Bool
canPromote Black _ c = getY c<=3
canPromote _ board coord = canPromote Black board$ reverseCoord board coord

reverseCoord :: Board a -> Coord -> Coord
reverseCoord (Board arr) c = max+min-c
    where (min, max) = bounds arr
