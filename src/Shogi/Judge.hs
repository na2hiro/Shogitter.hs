module Shogi.Judge
    ( Result(..)
    , NormalJudge
    , AbsentJudge
    , MateJudge
    , TryJudge
    , CheckMateJudge
    , OthelloJudge
    , GomokuJudge
    , WinHandCountJudge
    ) where

import {-# SOURCE #-}Shogi
import Board
import Coord
import Coord.Const(eightDirections)
import Color
import Piece(Piece(..), Kind(OU))
import Hands(removeFromHands, toList)
import Data.List(foldl')
import Control.Monad(foldM)
import Control.Arrow((***))

import Debug.Trace(trace)

data NormalJudge
instance Judge NormalJudge where
    judge _ = Nothing

data AbsentJudge
instance Judge AbsentJudge where
    judge (Shogi nextTurn board hands)
        | Nothing <- ouTurn = Just$ Win nextTurn
        | Nothing <- ouNextTurn = Just$ Win turn
        | otherwise = Nothing
        where (ouTurn, ouNextTurn) = orderTuple turn$ getOu board
              turn = opposite nextTurn

getOu :: Board m e a s mp -> (Maybe (Coord, Cell), Maybe (Coord, Cell))
getOu board = foldl' f (Nothing, Nothing)$ cells board
    where f (_, w) pos@(_, Just (Piece Black _ OU)) = (Just pos, w)
          f (b, _) pos@(_, Just (Piece White _ OU)) = (b, Just pos)
          f acc _ = acc

orderTuple :: Color -> (a, a) -> (a, a)
orderTuple Black a = a
orderTuple _ (b, w) = (w, b)

pickTuple :: Color -> (a, a) -> a
pickTuple Black (a, _) = a
pickTuple _ (_, a) = a

data MateJudge
instance Judge MateJudge where
    judge shogi@(Shogi nextTurn board hands)
        | mateTurn = Just$ Win nextTurn
        | mateNextTurn = Just$ Win turn
        | otherwise = Nothing
        where turn = opposite nextTurn
              (mateTurn, mateNextTurn) = orderTuple turn$ mate shogi

-- Mate to each color's OU
-- Precondition: there's 1 OU for each side
mate :: Shogi m e a s mp j -> (Bool, Bool)
mate (Shogi _ board hands) = (coordBlack `elem` tosWhite, coordWhite `elem` tosBlack)
        where (tosBlack, tosWhite) = getTo *** getTo$ getMovesEach board ([], [])
              getTo = map (\(Move _ to _)->to)
              (Just (coordBlack, _), Just (coordWhite, _)) = getOu board

-- Note: No support for multiple OU
data CheckMateJudge
instance Judge CheckMateJudge where
    judge shogi@(Shogi nextTurn board hands)
        | mateTurn = Just$ Win nextTurn
        | mateNextTurn && all (pickTuple nextTurn. mate) (getNextWithoutJudge shogi) = Just$ Win turn
        | otherwise = Nothing
        where turn = opposite nextTurn
              (mateTurn, mateNextTurn) = orderTuple turn$ mate shogi

-- TODO: Need to be combined with other rules. Error after OU is caught
data TryJudge
instance Judge TryJudge where
    judge shogi@(Shogi nextTurn board hands)
        | tryNextTurn = Just$ Win nextTurn
        | tryTurn = Just$ Win turn
        | otherwise = Nothing
        where turn = opposite nextTurn
              (Just (coordBlack, _), Just (coordWhite, _)) = getOu board
              (tryTurn, tryNextTurn) = orderTuple turn (coordBlack == Coord 5 1, coordWhite == Coord 5 9)

data OthelloJudge
instance Judge OthelloJudge where
    judge (Shogi _ b _) = compare <$> countM b
        where compare (b, w) | b>w = Win Black
                             | b<w = Win White
                             | otherwise = Even

countM :: Board m e a s mp -> Maybe (Int, Int)
countM board = foldM f (0, 0)$ cells board
    where f acc (_, Nothing) = Nothing
          f (b, w) (_, Just (Piece Black _ _)) = Just (b+1, w)
          f (b, w) _ = Just (b, w+1)

data GomokuJudge
instance Judge GomokuJudge where
    judge (Shogi nextTurn board _) | winNextTurn = Just$ Win nextTurn
                                   | winTurn = Just$ Win turn
                                   | otherwise = Nothing
        where turn = opposite nextTurn
              winNextTurn = any (fiveStreak. map (isColor nextTurn)) sliceAll
              winTurn = any (fiveStreak. map (isColor turn)) sliceAll
              sliceAll :: [[(Coord, Cell)]]
              sliceAll = do
                 x <- [1..9]
                 y <- [1..9]
                 vec <- eightDirections
                 return$ sliceFinite board (Coord x y) vec
              isColor :: Color -> (Coord, Cell) -> Bool
              isColor color (_, Just (Piece color' _ _)) | color==color' = True
              isColor _ _ = False


fiveStreak :: [Bool] -> Bool
fiveStreak = f 0
    where f n _ | n>=5 = True
          f n [] = False
          f n (True:xs) = f (n+1) xs
          f n (False:xs) = f 0 xs

data WinHandCountJudge
instance Judge WinHandCountJudge where
    judge (Shogi nextTurn _ hands)
        | win nextTurn = Just$ Win nextTurn
        | win turn = Just$ Win turn
        | otherwise = Nothing
        where turn = opposite nextTurn
              count hand = foldl' (+) 0$ map snd hand
              win t = count (toList t hands) >= 3

-- TODO: It's meaningless unless this is combined with other rules
data LoseHandCountJudge
