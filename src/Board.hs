module Board where

--import Array
import Piece
import Data.Array.IArray(Array, listArray, (!), (//), bounds)
import Data.List(transpose)
import Data.Ix as Ix(inRange)
import Data.Maybe(maybeToList)
import Control.Monad(guard)

type Masu = Maybe Piece
data Move = Move Coord Coord Promoted | Put Coord Kind deriving (Show, Eq)
data Board = Board (Array (Int,Int) Masu) deriving (Eq)
instance Show Board where
    show board = concat$ do
        y <- [1..9]
        return. (\line->"P"++show y++line++"\n"). concat$ do
            x <- [9,8..1]
            return. showMasu$ get (x,y) board
        where showMasu Nothing = " * "
              showMasu (Just p) = show p

initialBoard :: Board
initialBoard = Board$ listArray ((1,1),(9,9))$ concat. transpose$ gote++replicate 3 four++sente
    where one = map Just $ [KY .. KI]++OU:[KI,GI .. KY]
          two = Nothing:Just KA:replicate 5 Nothing++[Just HI,Nothing]
          three = replicate 9 $ Just FU
          four = replicate 9 Nothing
          gote :: [[Masu]]
          gote =  map (map (fmap (Piece White False))) [one,two,three]
          sente :: [[Masu]]
          sente = reverse$ map (reverse . map (fmap (\(Piece color _ kind)->Piece Black False kind))) gote

get :: Coord -> Board -> Masu
get (x,y) (Board b) = b!(x,y)

set :: (Coord, Masu) -> Board -> Board
set cp = sets [cp]

sets :: [(Coord, Masu)] -> Board -> Board
sets cps (Board b) = Board$ b // cps

inRange :: Board -> Coord -> Bool
inRange (Board arr) = Ix.inRange (bounds arr)

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


canPromote :: Color -> Board -> Coord -> Bool
canPromote Black _ (_,y) = y<=3
canPromote _ board coord = canPromote Black board$ reverseCoord board coord

reverseCoord :: Board -> Coord -> Coord
reverseCoord (Board arr) (x,y) = (xMax-x+xMin, yMax-y+yMin)
    where ((xMin, yMin), (xMax, yMax)) = bounds arr
