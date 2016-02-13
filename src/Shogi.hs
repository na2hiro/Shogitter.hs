{-# LANGUAGE GADTs #-}
module Shogi where

import Board
import Board.AbilityProxy(NormalAbilityProxy)
import Piece
import Hands
import Color
import Control.Monad(guard)

type Turn = Color

data Shogi a where
    Shogi :: AbilityProxy a => Turn -> Board a -> Hands -> Shogi a
instance Eq (Shogi a) where
    Shogi t b h == Shogi t' b' h' = t==t' && b==b' && h==h'
instance Show (Shogi a) where
    show (Shogi turn board hands) = show board ++ show hands ++ show turn ++ "\n"

type NormalShogi = Shogi NormalAbilityProxy

initialShogi :: AbilityProxy a => Shogi a
initialShogi = Shogi Black initialBoard initialHands

getMoves :: Shogi a -> [Move]
getMoves (Shogi turn board hands) = do
    (from, piece) <- cells board
    case piece of
        Nothing -> map (Put from) kinds
        Just (Piece color _ _) -> do
            guard$ color == turn
            dest <- destinationsAt from board
            if canPromote color board from || canPromote color board dest
                then map (Move from dest) [True,False]
                else return$ Move from dest False
    where kinds = kindsHand turn hands

getNext :: Shogi a -> [Shogi a]
getNext board = [unsafeDoMove move board | move <- getMoves board]

unsafeDoMove :: Move -> Shogi a -> Shogi a
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

doMove :: Move -> Shogi a -> Maybe (Shogi a)
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

