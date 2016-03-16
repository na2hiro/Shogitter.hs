{-# LANGUAGE GADTs #-}
module Shogi where

import Board(BoardI(..), NormalBoard, Mover, move, Effector, AbilityProxy, Slicer, MoverPredicator, Move(..), initialBoard, getMoves, isLegalMove)
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Slicer(NormalSlicer)
import Board.Mover(NormalMover)
import Board.Effector(NormalEffector)
import Board.MoverPredicator(NormalMoverPredicator)
import Piece
import Hands
import Color
import Control.Monad(guard)

type Turn = Color

data Shogi where
    Shogi :: BoardI b => Turn -> b -> Hands -> Shogi
{-instance Eq Shogi where
    Shogi t b h == Shogi t' b' h' = t==t' && b==b' && h==h'-}
instance Show Shogi where
    show (Shogi turn board hands) = show board ++ show hands ++ show turn ++ "\n"

normalShogi :: Shogi
normalShogi = Shogi Black (initialBoard :: NormalBoard) initialHands

getMovesShogi :: Shogi -> [Move]
getMovesShogi (Shogi turn board hands) = getMovesI turn board$ kindsHand turn hands

getNext :: Shogi -> [Shogi]
getNext shogi = [unsafeDoMove move shogi | move <- getMovesShogi shogi]

unsafeDoMove :: Move -> Shogi -> Shogi
unsafeDoMove mv@Move{} (Shogi turn board hands) = Shogi turn' board' hands'
    where turn' = opposite turn
          (board', kinds) = moveI turn mv board
          hands' = foldr (addToHands turn) hands kinds
unsafeDoMove mv@(Put _ kind) (Shogi turn board hands) = Shogi turn' board' hands'
    where turn' = opposite turn
          (board', kinds) = moveI turn mv board
          Just hands' = removeFromHands turn kind$ foldr (addToHands turn) hands kinds

doMove :: Move -> Shogi -> Maybe Shogi
doMove move shogi@(Shogi _ board _) = do
    guard$ isLegalMoveI board move
    return$ unsafeDoMove move shogi
doMove move@(Put _ kind) shogi@(Shogi turn board hands) = do
    guard$ isLegalMoveI board move
    _ <- removeFromHands turn kind hands
    return$ unsafeDoMove move shogi
