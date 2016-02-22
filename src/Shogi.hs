{-# LANGUAGE GADTs #-}
module Shogi where

import Board
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Slicer(NormalSlicer)
import Shogi.MoverPredicator(NormalMoverPredicator)
import Piece
import Hands
import Color
import Control.Monad(guard)

type Turn = Color

data Shogi a s mp where
    Shogi :: (AbilityProxy a, Slicer s, MoverPredicator mp) => Turn -> Board a s -> Hands -> Shogi a s mp
instance Eq (Shogi a s mp) where
    Shogi t b h == Shogi t' b' h' = t==t' && b==b' && h==h'
instance Show (Shogi a s mp) where
    show (Shogi turn board hands) = show board ++ show hands ++ show turn ++ "\n"

class MoverPredicator mp where
    canMove :: Shogi a s mp -> (Coord, Piece) -> Bool
    canMoveCoord :: Shogi a s mp -> Coord -> Bool
    canMoveCoord shogi@(Shogi _ board _) coord = canMove shogi (coord, unsafeGet coord board)

type NormalShogi = Shogi NormalAbilityProxy NormalSlicer NormalMoverPredicator

initialShogi :: (AbilityProxy a, Slicer s, MoverPredicator mp) => Shogi a s mp
initialShogi = Shogi Black initialBoard initialHands

getMoves :: Shogi a s mp -> [Move]
getMoves shogi@(Shogi turn board hands) = do
    (from, cell) <- cells board
    case cell of
        Nothing -> map (Put from) kinds
        Just p@(Piece color _ _) -> do
            guard$ canMove' (from, p)
            dest <- destinationsAt from board
            if canPromote color board from || canPromote color board dest
                then map (Move from dest) [True,False]
                else return$ Move from dest False
    where kinds = kindsHand turn hands
          canMove' = canMove shogi

getNext :: Shogi a s mp -> [Shogi a s mp]
getNext board = [unsafeDoMove move board | move <- getMoves board]

unsafeDoMove :: Move -> Shogi a s mp -> Shogi a s mp
unsafeDoMove (Move from to promoted) (Shogi turn board hands) = Shogi turn' board' hands'
    where Just fromPiece =  get from board
          fromPiece' = Just$ promote promoted fromPiece
          turn' = opposite turn
          board' = sets [(from, Nothing), (to, fromPiece')] board
          hands' = case get to board of
            Nothing -> hands
            Just (Piece _ _ kind) -> addToHands turn kind hands
unsafeDoMove (Put to kind) (Shogi turn board hands) = Shogi (opposite turn) board' hands'
    where Just hands' = removeFromHands turn kind hands
          board' = set (to, Just$ Piece turn False kind) board

doMove :: Move -> Shogi a s mp -> Maybe (Shogi a s mp)
doMove (Move from to promoted) (Shogi turn board hands) = do
    guard$ board `inRange` from
    guard$ board `inRange` to
    fromPiece@(Piece color _ _) <- get from board
    guard$ turn==color
    hands' <- case get to board of
        Nothing -> return hands
        Just (Piece color _ kind) -> if color==turn
            then Nothing
            else Just$ addToHands turn kind hands
    let fromPiece' = Just$ promote promoted fromPiece
    let turn' = opposite turn
    let board' = sets [(from, Nothing), (to, fromPiece')] board
    return$ Shogi turn' board' hands'
doMove (Put to kind) (Shogi turn board hands) = do
    guard$ board `inRange` to
    hands' <- removeFromHands turn kind hands
    case get to board of
        Nothing -> return$ Shogi (opposite turn) board' hands'
        Just _ -> Nothing
    where board' = set (to, Just$ Piece turn False kind) board

