{-# LANGUAGE GADTs #-}
module Shogi where

import Board(Board, Mover, move, Effector, AbilityProxy, Slicer, MoverPredicator, Move(..), initialBoard, getMoves, isLegalMove)
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Slicer(NormalSlicer)
import Board.Mover(NormalMover)
import Board.Effector(NormalEffector)
import Board.MoverPredicator(NormalMoverPredicator)
import Shogi.Judge(NormalJudge)
import Piece
import Hands
import Color
import Control.Monad(guard)

type Turn = Color

data Result = Win Color
            | Even
            deriving (Show, Eq)

data Shogi m e a s mp j where
    Shogi :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp, Judge j) => Turn -> Board m e a s mp -> Hands -> Shogi m e a s mp j
instance Eq (Shogi m e a s mp j) where
    Shogi t b h == Shogi t' b' h' = t==t' && b==b' && h==h'
instance Show (Shogi m e a s mp j) where
    show (Shogi turn board hands) = show board ++ show hands ++ show turn ++ "\n"

class Judge j where
    judge :: Shogi m e a s mp j -> Maybe Result

type NormalShogi = Shogi NormalMover NormalEffector NormalAbilityProxy NormalSlicer NormalMoverPredicator NormalJudge

initialShogi :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp, Judge j) => Shogi m e a s mp j
initialShogi = Shogi Black initialBoard initialHands

getMovesShogi :: Shogi m e a s mp j -> [Move]
getMovesShogi (Shogi turn board hands) = getMoves turn board$ kindsHand turn hands

getNext :: Shogi m e a s mp j -> [Shogi m e a s mp j]
getNext shogi = [unsafeDoMove move shogi | move <- getMovesShogi shogi]

getNextWithoutJudge :: Shogi m e a s mp j -> [Shogi m e a s mp j]
getNextWithoutJudge = getNext -- TODO: exclude judge

unsafeDoMove :: Move -> Shogi m e a s mp j -> Shogi m e a s mp j
unsafeDoMove mv (Shogi turn board hands) = Shogi turn' board' hands'
    where turn' = opposite turn
          (board', kinds) = move turn mv board
          hands' = foldr (addToHands turn) hands kinds

doMove :: Move -> Shogi m e a s mp j -> Maybe (Shogi m e a s mp j)
doMove move@Move{} shogi@(Shogi _ board _) = do
    guard$ isLegalMove board move
    return$ unsafeDoMove move shogi
doMove move@(Put _ kind) shogi@(Shogi turn board hands) = do
    guard$ isLegalMove board move
    _ <- removeFromHands turn kind hands
    return$ unsafeDoMove move shogi
