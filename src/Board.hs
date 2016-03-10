{-# LANGUAGE GADTs #-}
module Board where

import Piece(Piece(..), Kind(..), Coord, Color(..), Promoted, moveDefs, MoveDef(..), uniqueMoveDef)
import Coord(Coord(..), getY, direct)
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Slicer(NormalSlicer)
import Board.Mover(NormalMover)
import Board.Effector(NormalEffector)
import Board.MoverPredicator(NormalMoverPredicator)
import Data.Vector as V(Vector, fromList, toList, (!), (//))
import Data.List(transpose, nub)
import Data.Ix as Ix(inRange)
import Control.Arrow(first)

type Cell = Maybe Piece
data Move = Move Coord Coord Promoted | Put Coord Kind deriving (Show, Eq)

data Board m e a s mp where
    Board :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp) =>
        (Int, Int) -> Vector Cell -> Board m e a s mp
instance Eq (Board m e a s mp) where
    Board size vec == Board size2 vec2 = size == size2 && toList vec == toList vec2
instance Show (Board m e a s mp) where
    show board = concat$ do
        y <- [1..9]
        return. (\line->"P"++show y++line++"\n"). concat$ do
            x <- [9,8..1]
            return. showCell$ get board$ Coord x y
        where showCell Nothing = " * "
              showCell (Just p) = show p

type NormalBoard = Board NormalMover NormalEffector NormalAbilityProxy NormalSlicer NormalMoverPredicator

class AbilityProxy a where
    abilityProxy :: Color -> Coord -> Board m e a s mp -> [(Promoted, Kind)]

class Slicer s where
    sliceAsCoord :: Board m e a s mp -> Coord -> Coord -> [Coord]
    slice :: Board m e a s mp -> Coord -> Coord -> [(Coord, Cell)]
    slice board base vec = map (\coord -> (coord, get board coord))$ sliceAsCoord board base vec
    regularity :: Board m e a s mp -> Bool

class Mover m where
    move :: Move -> Board m e a s mp -> (Board m e a s mp, [Kind])

class Effector e where
    effect :: Coord -> Coord -> Board m e a s mp -> Board m e a s mp
    effectPut :: Coord -> Board m e a s mp -> Board m e a s mp
    effectPut = effect (error "Define `effectPut` for Effector if `effect` uses `from` parameter")

class MoverPredicator mp where
    canMove :: Board m e a s mp -> (Coord, Piece) -> Bool
    canMoveCoord :: Board m e a s mp -> Coord -> Bool
    canMoveCoord board coord = canMove board (coord, unsafeGet board coord)

initialBoard :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp) => Board m e a s mp
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

unsafeGet :: Board m e a s mp -> Coord -> Piece
unsafeGet b c = let Just p = get b c in p

get :: Board m e a s mp -> Coord -> Cell
get b@(Board size v) c = v!coordToInt b c

safeGet :: Board m e a s mp -> Coord -> Cell
safeGet b@(Board _ v) c = if b `Board.inRange` c then get b c else Nothing

set :: Board m e a s mp -> (Coord, Cell) -> Board m e a s mp
set b cp = sets b [cp]

sets :: Board m e a s mp -> [(Coord, Cell)] -> Board m e a s mp
sets b@(Board size v) cps = Board size$ v // map (first (coordToInt b)) cps

inRange :: Board m e a s mp -> Coord -> Bool
inRange board = Ix.inRange$ bounds board

bounds :: Board m e a s mp -> (Coord, Coord)
bounds (Board size vec) = (Coord 1 1, uncurry Coord size)

cells :: Board m e a s mp -> [(Coord, Cell)]
cells b@(Board _ vec) = zip (map (intToCoord b) [0..])$ toList vec

coordToInt :: Board m e a s mp -> Coord -> Int
coordToInt (Board (xMax, _) _) (Coord x y) = xMax*(x-1)+y-1
intToCoord :: Board m e a s mp -> Int -> Coord
intToCoord (Board (xMax, _) _) n = Coord ((n`div`xMax)+1)$ (n`rem`xMax)+1

destinationsAt :: Board m e a s mp -> Coord -> [Coord]
destinationsAt board@(Board _ _) from | regularity board = destinationsAt' board from
                                      | otherwise = nub$ destinationsAt' board from

destinationsAt' :: Board m e a s mp -> Coord -> [Coord]
destinationsAt' board@(Board _ _) from = do
    moveDef <- case abilities of
        [(promoted, ability)] -> moveDefs ability promoted
        _ -> uniqueMoveDef$ concat [moveDefs ability promoted|(promoted, ability)<-abilities]
    case moveDef of
        Exact vec -> take 1$ dests from (direct color vec)
        Slide vec -> dests from (direct color vec)
    where Piece color _ kind = unsafeGet board from
          abilities = abilityProxy color from board
          dests :: Coord -> Coord -> [Coord]
          dests from vec = takeW$ slice board from vec
          takeW :: [(Coord, Cell)] -> [Coord]
          takeW [] = []
          takeW ((to, Just (Piece color' _ _)):_) | color==color' = []
                                                  | otherwise = [to]
          takeW ((to,_):xs) = to:takeW xs

addCoord :: Color -> Coord -> Coord -> Coord
addCoord Black x v = x+v
addCoord White x v = x-v

canPromote :: Color -> Board m e a s mp -> Coord -> Bool
canPromote Black _ c = getY c<=3
canPromote _ board coord = canPromote Black board$ reverseCoord board coord

reverseCoord :: Board m e a s mp -> Coord -> Coord
reverseCoord board c = max+min-c
    where (min, max) = bounds board
