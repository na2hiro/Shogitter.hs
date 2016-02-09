module Board where

import Piece(Piece(..), Kind(..), Coord, Color(..), Promoted, move, MoveDef(..))
import Coord(Coord(..), getX, getY, toTuple)
import Data.Array.IArray(Array, listArray, (!), (//), bounds, indices, assocs)
import Data.List(transpose)
import Data.Ix as Ix(inRange)
import Data.Maybe(maybeToList, isJust, fromJust)
import Control.Monad(guard)
import Control.Arrow(second)

type Cell = Maybe Piece
data Move = Move Coord Coord Promoted | Put Coord Kind deriving (Show, Eq)
data Board = Board (Array Coord Cell) deriving (Eq)
instance Show Board where
    show board = concat$ do
        y <- [1..9]
        return. (\line->"P"++show y++line++"\n"). concat$ do
            x <- [9,8..1]
            return. showCell$ get (Coord x y) board
        where showCell Nothing = " * "
              showCell (Just p) = show p

initialBoard :: Board
initialBoard = Board$ listArray (Coord 1 1, Coord 9 9)$ concat. transpose$ gote++replicate 3 four++sente
    where one = map Just $ [KY .. KI]++OU:[KI,GI .. KY]
          two = Nothing:Just KA:replicate 5 Nothing++[Just HI,Nothing]
          three = replicate 9 $ Just FU
          four = replicate 9 Nothing
          gote :: [[Cell]]
          gote =  map (map (fmap (Piece White False))) [one,two,three]
          sente :: [[Cell]]
          sente = reverse$ map (reverse . map (fmap (\(Piece color _ kind)->Piece Black False kind))) gote

get :: Coord -> Board -> Cell
get c (Board b) = b!c

set :: (Coord, Cell) -> Board -> Board
set cp = sets [cp]

sets :: [(Coord, Cell)] -> Board -> Board
sets cps (Board b) = Board$ b // cps

inRange :: Board -> Coord -> Bool
inRange (Board arr) = Ix.inRange (bounds arr)

coords :: Board -> [Coord]
coords (Board arr) = indices arr

cells :: Board -> [(Coord, Cell)]
cells (Board arr) = assocs arr

pieces :: Board -> [(Coord, Piece)]
pieces = catSndMaybes. cells

catSndMaybes :: [(a, Maybe b)] -> [(a, b)]
catSndMaybes = map (second fromJust). filter (isJust. snd)

destinationsAt :: Coord -> Board -> [Coord]
destinationsAt from board = do
    Piece color promoted kind <- maybeToList$ get from board
    moveDef <- move kind promoted
    case moveDef of
        Exact vec -> take 1$ dests color from vec
        Slide vec -> dests color from vec
    where dests :: Color -> Coord -> Coord -> [Coord]
          dests color from vec = do
             let to = addCoord color vec from
             guard$ board `Board.inRange` to
             case get to board of
                 Just (Piece color' _ _) | color==color' -> []
                                         | otherwise -> return to
                 _ -> to : dests color to vec

addCoord :: Color -> Coord -> Coord -> Coord
addCoord Black v x = x+v
addCoord White v x = x-v

canPromote :: Color -> Board -> Coord -> Bool
canPromote Black _ c = getY c<=3
canPromote _ board coord = canPromote Black board$ reverseCoord board coord

reverseCoord :: Board -> Coord -> Coord
reverseCoord (Board arr) c = max+min-c
    where (min, max) = bounds arr
