{-# LANGUAGE GADTs #-}
module Shogi where

import Board
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Slicer(NormalSlicer)
import Board.Mover(NormalMover)
import Shogi.MoverPredicator(NormalMoverPredicator)
import Piece
import Hands
import Color
import Control.Monad(guard)
import Data.Vector.Generic(Vector)

type Turn = Color

data Shogi m a s mp where
    Shogi :: (Mover m, AbilityProxy a, Slicer s, MoverPredicator mp) => Turn -> Board m a s -> Hands -> Shogi m a s mp
instance Eq (Shogi m a s mp) where
    Shogi t b h == Shogi t' b' h' = t==t' && b==b' && h==h'
instance Show (Shogi m a s mp) where
    show (Shogi turn board hands) = show board ++ show hands ++ show turn ++ "\n"

class MoverPredicator mp where
    canMove :: Shogi m a s mp -> (Coord, Piece) -> Bool
    canMoveCoord :: Shogi m a s mp -> Coord -> Bool
    canMoveCoord shogi@(Shogi _ board _) coord = canMove shogi (coord, unsafeGet coord board)

type NormalShogi = Shogi NormalMover NormalAbilityProxy NormalSlicer NormalMoverPredicator

initialShogi :: (Mover m, AbilityProxy a, Slicer s, MoverPredicator mp) => Shogi m a s mp
initialShogi = Shogi Black initialBoard initialHands

getMoves :: Shogi m a s mp -> [Move]
getMoves shogi@(Shogi turn board hands) = do
    (from, cell) <- cells board
    case cell of
        Nothing -> map (Put from) kinds
        Just p@(Piece color _ _) -> getMovesFrom' shogi from p
    where kinds = kindsHand turn hands

getMovesFrom :: Shogi m a s mp -> Coord -> [Move]
getMovesFrom shogi@(Shogi _ board _) from = getMovesFrom' shogi from (unsafeGet from board)

getMovesFrom' :: Shogi m a s mp -> Coord -> Piece -> [Move]
getMovesFrom' shogi@(Shogi _ board _) from p@(Piece color _ _) = do
    guard$ canMove' (from, p)
    dest <- destinationsAt from board
    if canPromote color board from || canPromote color board dest
        then map (Move from dest) [True,False]
        else return$ Move from dest False
    where canMove' = canMove shogi

getNext :: Shogi m a s mp -> [Shogi m a s mp]
getNext board = [unsafeDoMove move board | move <- getMoves board]

unsafeDoMove :: Move -> Shogi m a s mp -> Shogi m a s mp
unsafeDoMove mv@(Move from to promoted) (Shogi turn board hands) = Shogi turn' board' hands'
    where turn' = opposite turn
          (board', kinds) = move mv board
          hands' = foldr (addToHands turn) hands kinds
unsafeDoMove (Put to kind) (Shogi turn board hands) = Shogi (opposite turn) board' hands'
    where Just hands' = removeFromHands turn kind hands
          board' = set (to, Just$ Piece turn False kind) board

doMove :: Move -> Shogi m a s mp -> Maybe (Shogi m a s mp)
doMove move@(Move from _ _) shogi@(Shogi _ board _) = do
    guard$ board `inRange` from
    guard$ move `elem` getMovesFrom shogi from
    return$ unsafeDoMove move shogi
doMove (Put to kind) (Shogi turn board hands) = do
    guard$ board `inRange` to
    hands' <- removeFromHands turn kind hands
    case get to board of
        Nothing -> return$ Shogi (opposite turn) board' hands'
        Just _ -> Nothing
    where board' = set (to, Just$ Piece turn False kind) board

