module Board where

import Piece(Piece(..), Kind(..), Coord, Color(..), Promoted, move, MoveDef(..), uniqueMoveDef)
import Coord(Coord(..), getX, getY, direct)
import Coord.Const(backward, forward, right, left)
import Data.Array.IArray(Array, listArray, (!), (//), bounds, indices, assocs)
import Data.List(transpose, nub)
import Data.Ix as Ix(inRange)
import Data.Maybe(maybeToList, isJust, fromJust, catMaybes, mapMaybe)
import Control.Monad(guard, mplus)
import Control.Arrow(second)

type Cell = Maybe Piece
data Move = Move Coord Coord Promoted | Put Coord Kind deriving (Show, Eq)
data Board = Board AbilityProxy (Array Coord Cell) deriving (Eq)
instance Show Board where
    show board = concat$ do
        y <- [1..9]
        return. (\line->"P"++show y++line++"\n"). concat$ do
            x <- [9,8..1]
            return. showCell$ get (Coord x y) board
        where showCell Nothing = " * "
              showCell (Just p) = show p

initialBoard :: Board
initialBoard = Board NormalAbilityProxy initialArray

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

unsafeGet :: Coord -> Board -> Piece
unsafeGet c b = let Just p = get c b in p

get :: Coord -> Board -> Cell
get c (Board _ b) = b!c

safeGet :: Coord -> Board ->  Cell
safeGet c b@(Board _ a) = if b `Board.inRange` c then a!c else Nothing

set :: (Coord, Cell) -> Board -> Board
set cp = sets [cp]

sets :: [(Coord, Cell)] -> Board -> Board
sets cps (Board ap b) = Board ap$ b // cps

inRange :: Board -> Coord -> Bool
inRange (Board _ arr) = Ix.inRange (bounds arr)

coords :: Board -> [Coord]
coords (Board _ arr) = indices arr

cells :: Board -> [(Coord, Cell)]
cells (Board _ arr) = assocs arr

pieces :: Board -> [(Coord, Piece)]
pieces = catSndMaybes. cells

catSndMaybes :: [(a, Maybe b)] -> [(a, b)]
catSndMaybes = map (second fromJust). filter (isJust. snd)

slice :: Coord -> Coord -> (Cell -> Bool) -> Board -> [Cell]
slice base vec cond board = if board `Board.inRange` next && cond cell
    then cell: slice next vec cond board
    else []
    where next = base + vec
          cell = get next board

destinationsAt :: Coord -> Board -> [Coord]
destinationsAt from board = do
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

canPromote :: Color -> Board -> Coord -> Bool
canPromote Black _ c = getY c<=3
canPromote _ board coord = canPromote Black board$ reverseCoord board coord

reverseCoord :: Board -> Coord -> Coord
reverseCoord (Board _ arr) c = max+min-c
    where (min, max) = bounds arr

{- ability proxy -}
data AbilityProxy = NormalAbilityProxy
                  | AnnanAbilityProxy
                  | AnhokuAbilityProxy
                  | AntouzaiAbilityProxy
                  | AnkiAbilityProxy
                  | TenjikuAbilityProxy
                  | NekosenAbilityProxy
                  | NekonekosenAbilityProxy
                  | YokoNekosenAbilityProxy
                  | YokoNekonekosenAbilityProxy
                  | TaimenAbilityProxy
                  | HaimenAbilityProxy
--                  | QuantumAbilityProxy
                  deriving (Eq, Show)

abilityProxy :: Color -> Coord -> Board -> [Kind]
abilityProxy _  c b@(Board NormalAbilityProxy _) = normalProxy c b
abilityProxy color c b@(Board AnnanAbilityProxy _)= maybeToList$ pieceToKind<$> safeGet back b `mplus` get c b
    where back = addCoord color backward c
abilityProxy color c b@(Board AnhokuAbilityProxy _)= maybeToList$ pieceToKind<$> safeGet for b `mplus` get c b
    where for = addCoord color forward c
abilityProxy color c b@(Board AntouzaiAbilityProxy _)= if null lr
    then normalProxy c b
    else map pieceToKind lr
    where lr = mapMaybe (flip safeGet b. (c+)) [left, right]
abilityProxy color c b@(Board AnkiAbilityProxy _)= if null p8
    then normalProxy c b
    else  map pieceToKind p8
    where happou = [c*unit|c<-[Coord 1 2, Coord 2 1], unit<-[Coord 1 1, Coord 1 (-1), Coord (-1) 1, Coord (-1) (-1)]]
          p8 = mapMaybe (flip safeGet b. flip (addCoord color) c) happou
abilityProxy color c b@(Board TenjikuAbilityProxy _) = map pieceToKind$ catMaybes [safeGet back b, get c b]
    where back = addCoord color backward c
abilityProxy color c b@(Board NekosenAbilityProxy _)= [pieceToKind$ unsafeGet (Coord (getX c) (getY c+backwards-forwards)) b]
    where forwards = length$ slice c forward cond b
          backwards = length$ slice c backward cond b
          cond (Just (Piece color' _ _)) | color==color' = True
          cond _ = False
abilityProxy color c b@(Board YokoNekosenAbilityProxy _)= [pieceToKind$ unsafeGet (Coord (getX c+lefts-rights) (getY c)) b]
    where rights = length$ slice c right cond b
          lefts = length$ slice c left cond b
          cond (Just (Piece color' _ _)) | color==color' = True
          cond _ = False
abilityProxy color c b@(Board NekonekosenAbilityProxy _)= [pieceToKind$ unsafeGet (Coord (getX c) (getY c+backwards-forwards)) b]
    where forwards = length$ slice c forward isJust b
          backwards = length$ slice c backward isJust b
abilityProxy color c b@(Board YokoNekonekosenAbilityProxy _)= [pieceToKind$ unsafeGet (Coord (getX c+lefts-rights) (getY c)) b]
    where rights = length$ slice c right isJust b
          lefts = length$ slice c left isJust b
abilityProxy color c b@(Board TaimenAbilityProxy _)= case safeGet (addCoord color forward c) b of
    Just (Piece color' _ kind) | color/=color' -> [kind]
    _ -> normalProxy c b
abilityProxy color c b@(Board HaimenAbilityProxy _)= case safeGet (addCoord color backward c) b of
    Just (Piece color' _ kind) | color/=color' -> [kind]
    _ -> normalProxy c b

normalProxy :: Coord -> Board -> [Kind]
normalProxy c b =return$ pieceToKind$ unsafeGet c b

pieceToKind (Piece _ _ kind) = kind
{- / ability proxy -}
