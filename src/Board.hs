module Board where

import Control.Arrow (first)
import Control.Monad (guard)
import Coord (Coord(..), direct, getY)
import Data.Ix as Ix (inRange)
import Data.List (nub, transpose)
import Data.Maybe (isNothing)
import Data.Vector as V (Vector, (!), (//), fromList, toList)
import Piece
  ( Color(..)
  , Coord
  , Kind(..)
  , MoveDef(..)
  , Piece(..)
  , Promoted
  , moveDefs
  , promotable
  , uniqueMoveDef
  )

type Cell = Maybe Piece

data Move
  = Move Coord
         Coord
         Promoted
  | Put Coord
        Kind
  deriving (Show, Eq)

data Board = Board
  { getMover :: Mover
  , getEffector :: Effector
  , getAbilityProxy :: AbilityProxy
  , getSlicer :: Slicer
  , getMoverPredicator :: MoverPredicator
  , size :: (Int, Int)
  , vector :: Vector Cell
  }

instance Eq Board where
  b1 == b2 = size b1 == size b2 && vector b1 == vector b2

instance Show Board where
  show board =
    concat $ do
      y <- [1 .. ySize]
      return . (\line -> "P" ++ show y ++ line ++ "\n") . concat $ do
        x <- [xSize,xSize - 1 .. 1]
        return . showCell $ get board $ Coord x y
    where
      showCell Nothing = " * "
      showCell (Just p) = show p
      (xSize, ySize) = size board

data Mover = Mover
  { moverId :: String
  , runMover :: Color -> Move -> Board -> (Board, [Kind])
  }

instance Eq Mover where
  a1 == a2 = moverId a1 == moverId a2

move :: Color -> Move -> Board -> (Board, [Kind])
move color move board = runMover (getMover board) color move board

data Effector = Effector
  { effectorId :: String
  , runEffector :: Coord -> Coord -> Board -> Board
  , runEffectorPut :: Coord -> Board -> Board
  }

instance Eq Effector where
  a1 == a2 = effectorId a1 == effectorId a2

effect :: Coord -> Coord -> Board -> Board
effect from to board = runEffector (getEffector board) from to board

effectPut :: Coord -> Board -> Board
effectPut to board = runEffectorPut (getEffector board) to board

data AbilityProxy = AbilityProxy
  { abilityProxyId :: String
  , runAbilityProxy :: Color -> Coord -> Board -> [(Promoted, Kind)]
  }

instance Eq AbilityProxy where
  a1 == a2 = abilityProxyId a1 == abilityProxyId a2

abilityProxy :: Color -> Coord -> Board -> [(Promoted, Kind)]
abilityProxy color from board =
  runAbilityProxy (getAbilityProxy board) color from board

data Slicer = Slicer
  { slicerId :: String
  , runSliceAsCoord :: Board -> Coord -> Coord -> [Coord]
  , runRegularity :: Bool
  }

instance Eq Slicer where
  a1 == a2 = slicerId a1 == slicerId a2

sliceAsCoord :: Board -> Coord -> Coord -> [Coord]
sliceAsCoord board base vec = runSliceAsCoord (getSlicer board) board base vec

regularity :: Board -> Bool
regularity = runRegularity . getSlicer

slice :: Board -> Coord -> Coord -> [(Coord, Cell)]
slice board base vec =
  map (\coord -> (coord, get board coord)) $ sliceAsCoord board base vec

sliceFinite :: Board -> Coord -> Coord -> [(Coord, Cell)]
sliceFinite board base vec =
  map (\coord -> (coord, get board coord)) $ sliceAsCoordFinite board base vec

sliceAsCoordFinite :: Board -> Coord -> Coord -> [Coord]
sliceAsCoordFinite board base vec =
  if regularity board || null slices
    then slices
    else f slices
  where
    slices = sliceAsCoord board base vec
    first = head slices
    f [x] = [x]
    f (x:y:xs)
      | base == x && first == y = [x]
    f (_:y:xs) = f (y : xs)

data MoverPredicator = MoverPredicator
  { moverPredicatorId :: String
  , runMoverPredicator :: Board -> (Coord, Piece) -> Bool
  }

instance Eq MoverPredicator where
  a1 == a2 = moverPredicatorId a1 == moverPredicatorId a2

canMove :: Board -> (Coord, Piece) -> Bool
canMove board pair = runMoverPredicator (getMoverPredicator board) board pair

canMoveCoord :: Board -> Coord -> Bool
canMoveCoord board coord = canMove board (coord, unsafeGet board coord)

initialArray :: Vector Cell
initialArray = fromList . concat . transpose $ gote ++ replicate 3 four ++ sente
  where
    one = map Just $ [KY .. KI] ++ OU : [KI,GI .. KY]
    two = Nothing : Just KA : replicate 5 Nothing ++ [Just HI, Nothing]
    three = replicate 9 $ Just FU
    four = replicate 9 Nothing
    gote :: [[Cell]]
    gote = map (map (fmap (Piece White False))) [one, two, three]
    sente :: [[Cell]]
    sente =
      reverse $
      map
        (reverse . map (fmap (\(Piece _ _ kind) -> Piece Black False kind)))
        gote

-- | Get a piece. This fails when there is no piece found
unsafeGet :: Board -> Coord -> Piece
unsafeGet b c =
  let Just p = get b c
   in p

get :: Board -> Coord -> Cell
get b c = vector b ! coordToInt b c

safeGet :: Board -> Coord -> Cell
safeGet b c =
  if b `Board.inRange` c
    then get b c
    else Nothing

set :: Board -> (Coord, Cell) -> Board
set b cp = sets b [cp]

sets :: Board -> [(Coord, Cell)] -> Board
sets b cps = b {vector = vector b // map (first (coordToInt b)) cps}

inRange :: Board -> Coord -> Bool
inRange board = Ix.inRange $ bounds board

bounds :: Board -> (Coord, Coord)
bounds b = (Coord 1 1, uncurry Coord (size b))

cells :: Board -> [(Coord, Cell)]
cells b = zip (map (intToCoord b) [0 ..]) $ toList $ vector b

coordToInt :: Board -> Coord -> Int
coordToInt board (Coord x y) = yMax * (x - 1) + y - 1
  where
    (_, yMax) = size board

intToCoord :: Board -> Int -> Coord
intToCoord board n = Coord ((n `div` yMax) + 1) $ (n `rem` yMax) + 1
  where
    (_, yMax) = size board

getMoves :: Color -> Board -> [Kind] -> [Move]
getMoves turn board kinds = do
  (from, cell) <- cells board
  case cell of
    Nothing -> map (Put from) kinds
    Just (Piece color _ _)
      | turn == color -> getMovesFrom board from
    _ -> []

getMovesEach :: Board -> ([Kind], [Kind]) -> ([Move], [Move])
getMovesEach board (kindsB, kindsW) = foldr f ([], []) $ cells board
  where
    f (from, Nothing) (bs, ws) =
      (map (Put from) kindsB ++ bs, map (Put from) kindsW ++ ws)
    f (from, Just (Piece Black _ _)) (bs, ws) =
      (getMovesFrom board from ++ bs, ws)
    f (from, Just (Piece White _ _)) (bs, ws) =
      (bs, getMovesFrom board from ++ ws)

-- | Get moves from coord. This fails when there is no piece found.
getMovesFrom :: Board -> Coord -> [Move]
getMovesFrom board from = getMovesFrom' board from $ unsafeGet board from

getMovesFrom' :: Board -> Coord -> Piece -> [Move]
getMovesFrom' board from p@(Piece color promoted kind) = do
  guard $ canMove board (from, p)
  dest <- destinationsAt board from
  let kindPromotable = promotable kind
  let coordPromotable =
        canPromote color board from || canPromote color board dest
  if not promoted && kindPromotable && coordPromotable
    then map (Move from dest) [True, False]
    else return $ Move from dest False

destinationsAt :: Board -> Coord -> [Coord]
destinationsAt board from
  | regularity board = destinationsAt' board from
  | otherwise = nub $ destinationsAt' board from

destinationsAt' :: Board -> Coord -> [Coord]
destinationsAt' board from = do
  moveDef <-
    case abilities of
      [(promoted, ability)] -> moveDefs ability promoted
      _ ->
        uniqueMoveDef $
        concat [moveDefs ability promoted | (promoted, ability) <- abilities]
  case moveDef of
    Exact vec -> take 1 $ dests from (direct color vec)
    Slide vec -> dests from (direct color vec)
  where
    Piece color _ _ = unsafeGet board from
    abilities = abilityProxy color from board
    dests :: Coord -> Coord -> [Coord]
    dests from vec = takeW $ slice board from vec
    takeW :: [(Coord, Cell)] -> [Coord]
    takeW [] = []
    takeW ((to, Just (Piece color' _ _)):_)
      | color == color' = []
      | otherwise = [to]
    takeW ((to, _):xs) = to : takeW xs

isLegalMove :: Board -> Move -> Bool
isLegalMove board move@(Move from _ _) =
  board `Board.inRange` from && move `elem` getMovesFrom board from
isLegalMove board (Put to _) =
  board `Board.inRange` to && isNothing (get board to)

addCoord :: Color -> Coord -> Coord -> Coord
addCoord Black x v = x + v
addCoord White x v = x - v

canPromote :: Color -> Board -> Coord -> Bool
canPromote Black _ c = getY c <= 3
canPromote _ board coord = canPromote Black board $ reverseCoord board coord

reverseCoord :: Board -> Coord -> Coord
reverseCoord board c = max + min - c
  where
    (min, max) = bounds board
{-
fromCsa :: String -> Board
-}
