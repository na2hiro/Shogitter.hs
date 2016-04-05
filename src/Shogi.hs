{-# LANGUAGE GADTs #-}
module Shogi where

import Board(Board, Mover, move, Effector, AbilityProxy, Slicer, MoverPredicator, Move(..), initialBoard, getMoves,
    isLegalMove, unsafeGet, get)
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Slicer(NormalSlicer)
import Board.Mover(NormalMover)
import Board.Effector(NormalEffector)
import Board.MoverPredicator(NormalMoverPredicator)
import Shogi.Judge(NormalJudge)
import Piece
import Hands
import Color
import Coord(Coord(..))
import Control.Monad(guard)

type Turn = Color

data Result = Win
            | Lose
            | Even
            deriving (Show, Eq)

data History = History [Diff] deriving (Eq)
instance Show History where
    show (History diffs) = "History " ++ show (reverse diffs)
data Diff = Diff DetailedMove deriving (Eq)
instance Show Diff where
    show (Diff detailedMove) = show detailedMove
data DetailedMove = DMove Color Coord Coord Kind Promoted (Maybe Kind)
                  | DPut Color Coord Kind deriving (Eq)
instance Show DetailedMove where
    show (DMove color from to kind promoted captured) = showCoord from ++ showCoord to ++ show kind
        ++ (if promoted then "*" else "") ++ (case captured of
            Just k -> "("++show k++")"
            Nothing -> "")
        where showCoord (Coord x y) = show x++show y

initialHistory :: History
initialHistory = History []

addHistory :: Shogi m e a s mp j -> Move -> History -> History
addHistory (Shogi _ turn board _) (Move from to promoted) (History diffs) = History$ Diff dMove: diffs
    where dMove = DMove turn from to (getKind$ unsafeGet board from) promoted (getKind <$> get board to)
          getKind (Piece _ _ kind) = kind
addHistory (Shogi _ turn board _) (Put to kind) (History diffs) = History$ Diff dPut: diffs
    where dPut = DPut turn to kind

data Shogi m e a s mp j where
    Shogi :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp, Judge j)
        => History -> Turn -> Board m e a s mp -> Hands -> Shogi m e a s mp j
instance Eq (Shogi m e a s mp j) where
    Shogi _ t b h == Shogi _ t' b' h' = t==t' && b==b' && h==h'
instance Show (Shogi m e a s mp j) where
    show (Shogi history turn board hands) = show board ++ show hands ++ show turn ++ show history ++ "\n"

class Judge j where
    judge :: Shogi m e a s mp j -> Maybe Result

type NormalShogi = Shogi NormalMover NormalEffector NormalAbilityProxy NormalSlicer NormalMoverPredicator NormalJudge

initialShogi :: (Mover m, Effector e, AbilityProxy a, Slicer s, MoverPredicator mp, Judge j) => Shogi m e a s mp j
initialShogi = Shogi initialHistory Black initialBoard initialHands

getMovesShogi :: Shogi m e a s mp j -> [Move]
getMovesShogi (Shogi _ turn board hands) = getMoves turn board$ kindsHand turn hands

getNext :: Shogi m e a s mp j -> [Shogi m e a s mp j]
getNext shogi = [unsafeDoMove move shogi | move <- getMovesShogi shogi]

getNextWithoutJudge :: Shogi m e a s mp j -> [Shogi m e a s mp j]
getNextWithoutJudge = getNext -- TODO: exclude judge

unsafeDoMove :: Move -> Shogi m e a s mp j -> Shogi m e a s mp j
unsafeDoMove mv@Move{} shogi@(Shogi history turn board hands) = Shogi history' turn' board' hands'
    where turn' = opposite turn
          (board', kinds) = move turn mv board
          hands' = foldr (addToHands turn) hands kinds
          history' = addHistory shogi mv history
unsafeDoMove mv@(Put _ kind) shogi@(Shogi history turn board hands) = Shogi history' turn' board' hands'
    where turn' = opposite turn
          (board', kinds) = move turn mv board
          Just hands' = removeFromHands turn kind$ foldr (addToHands turn) hands kinds
          history' = addHistory shogi mv history

doMove :: Move -> Shogi m e a s mp j -> Maybe (Shogi m e a s mp j)
doMove move@Move{} shogi@(Shogi _ _ board _) = do
    guard$ isLegalMove board move
    return$ unsafeDoMove move shogi
doMove move@(Put _ kind) shogi@(Shogi _ turn board hands) = do
    guard$ isLegalMove board move
    _ <- removeFromHands turn kind hands
    return$ unsafeDoMove move shogi
