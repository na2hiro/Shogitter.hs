{-# LANGUAGE GADTs #-}
module Shogi where

import Board
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Slicer(NormalSlicer)
import Board.Mover(NormalMover)
import Board.Effector(NormalEffector)
import Board.MoverPredicator(NormalMoverPredicator)
import Piece
import Hands
import Color
import Control.Monad(guard)
import Data.Vector.Generic(Vector)
import Data.Maybe(isNothing)

type Turn = Color

data Shogi m e a s mp where
    Shogi :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp) => Turn -> Board m e a s mp -> Hands -> Shogi m e a s mp
instance Eq (Shogi m e a s mp) where
    Shogi t b h == Shogi t' b' h' = t==t' && b==b' && h==h'
instance Show (Shogi m e a s mp) where
    show (Shogi turn board hands) = show board ++ show hands ++ show turn ++ "\n"


type NormalShogi = Shogi NormalMover NormalEffector NormalAbilityProxy NormalSlicer NormalMoverPredicator

initialShogi :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp) => Shogi m e a s mp
initialShogi = Shogi Black initialBoard initialHands

getMoves :: Shogi m e a s mp -> [Move]
getMoves shogi@(Shogi turn board hands) = do
    (from, cell) <- cells board
    case cell of
        Nothing -> map (Put from) kinds
        Just p@(Piece color _ _) -> getMovesFrom' shogi from p
    where kinds = kindsHand turn hands

getMovesFrom :: Shogi m e a s mp -> Coord -> [Move]
getMovesFrom shogi@(Shogi _ board _) from = getMovesFrom' shogi from$ unsafeGet board from

getMovesFrom' :: Shogi m e a s mp -> Coord -> Piece -> [Move]
getMovesFrom' shogi@(Shogi turn board _) from p@(Piece color _ _) = do
    guard$ color == turn
    guard$ canMove board (from, p)
    dest <- destinationsAt board from
    if canPromote color board from || canPromote color board dest
        then map (Move from dest) [True,False]
        else return$ Move from dest False

getNext :: Shogi m e a s mp -> [Shogi m e a s mp]
getNext board = [unsafeDoMove move board | move <- getMoves board]

unsafeDoMove :: Move -> Shogi m e a s mp -> Shogi m e a s mp
unsafeDoMove mv@(Move from to promoted) (Shogi turn board hands) = Shogi turn' (effect from to board') hands'
    where turn' = opposite turn
          (board', kinds) = move mv board
          hands' = foldr (addToHands turn) hands kinds
unsafeDoMove (Put to kind) (Shogi turn board hands) = Shogi (opposite turn) (effectPut to board') hands'
    where Just hands' = removeFromHands turn kind hands
          board' = set board (to, Just$ Piece turn False kind)

doMove :: Move -> Shogi m e a s mp -> Maybe (Shogi m e a s mp)
doMove move@(Move from _ _) shogi@(Shogi _ board _) = do
    guard$ board `inRange` from && move `elem` getMovesFrom shogi from
    return$ unsafeDoMove move shogi
doMove move@(Put to kind) shogi@(Shogi turn board hands) = do
    guard$ board `inRange` to && isNothing (get board to)
    hands' <- removeFromHands turn kind hands
    return$ unsafeDoMove move shogi
