{-# LANGUAGE GADTs #-}
module Board where

import Piece(Piece(..), Kind(..), Coord, Color(..), Promoted, move, MoveDef(..), uniqueMoveDef)
import Coord(Coord(..), getY, direct)
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Slicer(NormalSlicer)
import Data.Vector as V(Vector, fromList, toList, (!), (//))
import Data.List(transpose, nub)
import Data.Ix as Ix(inRange)
import Control.Arrow(first)

type Cell = Maybe Piece
data Move = Move Coord Coord Promoted | Put Coord Kind deriving (Show, Eq)

data Board a s where
    Board :: (AbilityProxy a, Slicer s) => (Int, Int) -> Vector Cell -> Board a s
instance Eq (Board a s) where
    Board size vec == Board size2 vec2 = size == size2 && toList vec == toList vec2
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
    abilityProxy :: Color -> Coord -> Board a s -> [(Promoted, Kind)]

class Slicer s where
    slice :: Coord -> Coord -> Board a s -> [Coord]
    regularity :: Board a s -> Bool

initialBoard :: (AbilityProxy a, Slicer s) => Board a s
initialBoard = Board (9,9) initialArray

initialArray :: Vector Cell
initialArray = fromList. concat. transpose$ gote++replicate 3 four++sente
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
get c b@(Board size v) = v!coordToInt b c

safeGet :: Coord -> Board a s -> Cell
safeGet c b@(Board _ v) = if b `Board.inRange` c then get c b else Nothing

set :: (Coord, Cell) -> Board a s -> Board a s
set cp = sets [cp]

sets :: [(Coord, Cell)] -> Board a s -> Board a s
sets cps b@(Board size v) = Board size$ v // map (first (coordToInt b)) cps

inRange :: Board a s -> Coord -> Bool
inRange board = Ix.inRange$ bounds board

bounds :: Board a s -> (Coord, Coord)
bounds (Board size vec) = (Coord 1 1, uncurry Coord size)

cells :: Board a s -> [(Coord, Cell)]
cells b@(Board _ vec) = zip (map (intToCoord b) [0..])$ toList vec

coordToInt :: Board a s -> Coord -> Int
coordToInt (Board (xMax, _) _) (Coord x y) = xMax*(x-1)+y-1
intToCoord :: Board a s -> Int -> Coord
intToCoord (Board (xMax, _) _) n = Coord ((n`div`xMax)+1)$ (n`rem`xMax)+1

destinationsAt :: Coord -> Board a s -> [Coord]
destinationsAt from board@(Board _ _) | regularity board = destinationsAt' from board
                                      | otherwise = nub$ destinationsAt' from board

destinationsAt' :: Coord -> Board a s -> [Coord]
destinationsAt' from board@(Board _ _) = do
    moveDef <- case abilities of
        [(promoted, ability)] -> move ability promoted
        _ -> uniqueMoveDef$ concat [move ability promoted|(promoted, ability)<-abilities]
    case moveDef of
        Exact vec -> take 1$ dests from (direct color vec)
        Slide vec -> dests from (direct color vec)
    where Piece color _ kind = unsafeGet from board
          abilities = abilityProxy color from board
          dests :: Coord -> Coord -> [Coord]
          dests from vec = takeW$ map (\coord->(coord, get coord board))$ slice from vec board
          takeW :: [(Coord, Cell)] -> [Coord]
          takeW [] = []
          takeW ((to, Just (Piece color' _ _)):_) | color==color' = []
                                                  | otherwise = [to]
          takeW ((to,_):xs) = to:takeW xs

addCoord :: Color -> Coord -> Coord -> Coord
addCoord Black v x = x+v
addCoord White v x = x-v

canPromote :: Color -> Board a s -> Coord -> Bool
canPromote Black _ c = getY c<=3
canPromote _ board coord = canPromote Black board$ reverseCoord board coord

reverseCoord :: Board a s -> Coord -> Coord
reverseCoord board c = max+min-c
    where (min, max) = bounds board
