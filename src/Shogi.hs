{-# LANGUAGE GADTs #-}
module Shogi where

import Board(Board, Mover, move, Effector, AbilityProxy, Slicer, MoverPredicator, Move(..), initialBoard, getMoves, isLegalMove)
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

data Shogi m e a s mp where
    Shogi :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp) => Turn -> Board m e a s mp -> Hands -> Shogi m e a s mp
instance Eq (Shogi m e a s mp) where
    Shogi t b h == Shogi t' b' h' = t==t' && b==b' && h==h'
instance Show (Shogi m e a s mp) where
    show (Shogi turn board hands) = show board ++ show hands ++ show turn ++ "\n"

type NormalShogi = Shogi NormalMover NormalEffector NormalAbilityProxy NormalSlicer NormalMoverPredicator

initialShogi :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp) => Shogi m e a s mp
initialShogi = Shogi Black initialBoard initialHands

getMovesShogi :: Shogi m e a s mp -> [Move]
getMovesShogi (Shogi turn board hands) = getMoves turn board$ kindsHand turn hands

getNext :: Shogi m e a s mp -> [Shogi m e a s mp]
getNext shogi = [unsafeDoMove move shogi | move <- getMovesShogi shogi]

unsafeDoMove :: Move -> Shogi m e a s mp -> Shogi m e a s mp
unsafeDoMove mv (Shogi turn board hands) = Shogi turn' board' hands'
    where turn' = opposite turn
          (board', kinds) = move turn mv board
          hands' = foldr (addToHands turn) hands kinds

doMove :: Move -> Shogi m e a s mp -> Maybe (Shogi m e a s mp)
doMove move shogi@(Shogi _ board _) = do
    guard$ isLegalMove board move
    return$ unsafeDoMove move shogi
doMove move@(Put _ kind) shogi@(Shogi turn board hands) = do
    guard$ isLegalMove board move
    _ <- removeFromHands turn kind hands
    return$ unsafeDoMove move shogi
