module Shogi where

import Board (Board, Move(..), get, getMoves, isLegalMove, move, unsafeGet)
import Color
import Control.Monad (guard)
import Coord (Coord(..))
import Hands
import Piece

type Turn = Color

data Result
  = Win
  | Lose
  | Even
  deriving (Show, Eq)

data History =
  History [Diff]
  deriving (Eq)

instance Show History where
  show (History diffs) = "History " ++ show (reverse diffs)

data Diff =
  Diff DetailedMove
  deriving (Eq)

instance Show Diff where
  show (Diff detailedMove) = show detailedMove

data DetailedMove
  = DMove Color
          Coord
          Coord
          Kind
          Promoted
          (Maybe (Kind, Promoted))
  | DPut Color
         Coord
         Kind
  deriving (Eq)

instance Show DetailedMove where
  show (DMove color from to kind promoted captured) =
    showCoord from ++
    showCoord to ++
    show kind ++
    showPromoted promoted ++
    (case captured of
       Just (k, promoted') -> "(" ++ show k ++ showPromoted promoted' ++ ")"
       Nothing -> "")
    where
      showCoord (Coord x y) = show x ++ show y
      showPromoted True = "*"
      showPromoted False = ""
  show (DPut color to kind) = showCoord to ++ show kind
    where
      showCoord (Coord x y) = show x ++ show y

initialHistory :: History
initialHistory = History []

addHistory :: Shogi -> Move -> History -> History
addHistory shogi (Move from to promoted) (History diffs) =
  History $ Diff dMove : diffs
  where
    b = board shogi
    dMove =
      DMove
        (turn shogi)
        from
        to
        (getKind $ unsafeGet b from)
        promoted
        (getPair <$> get b to)
    getKind (Piece _ _ kind) = kind
    getPair (Piece _ promoted kind) = (kind, promoted)
addHistory shogi (Put to kind) (History diffs) = History $ Diff dPut : diffs
  where
    dPut = DPut (turn shogi) to kind

data Shogi = Shogi
  { getJudge :: Judge
  , history :: History
  , turn :: Turn
  , board :: Board
  , hands :: Hands
  }

instance Eq Shogi where
  s == s' = turn s == turn s' && board s == board s' && hands s == hands s'

instance Show Shogi where
  show s =
    show (board s) ++
    show (hands s) ++ show (turn s) ++ show (history s) ++ "\n"

data Judge = Judge
  { judgeId :: String
  , runJudge :: Shogi -> Maybe Result
  }

judge :: Shogi -> Maybe Result
judge shogi = runJudge (getJudge shogi) shogi

getMovesShogi :: Shogi -> [Move]
getMovesShogi shogi =
  getMoves currentTurn (board shogi) $ kindsHand currentTurn $ hands shogi
  where
    currentTurn = turn shogi

getNext :: Shogi -> [Shogi]
getNext shogi = [unsafeDoMove move shogi | move <- getMovesShogi shogi]

unsafeDoMove :: Move -> Shogi -> Shogi
unsafeDoMove mv@Move {} shogi =
  shogi
    { history = addHistory shogi mv $ history shogi
    , turn = opposite currentTurn
    , board = board'
    , hands = foldr (addToHands currentTurn) (hands shogi) kinds
    }
  where
    currentTurn = turn shogi
    (board', kinds) = move currentTurn mv $ board shogi
unsafeDoMove mv@(Put _ kind) shogi =
  shogi
    { history = addHistory shogi mv $ history shogi
    , turn = opposite currentTurn
    , board = board'
    , hands = hands'
    }
  where
    currentTurn = turn shogi
    (board', kinds) = move currentTurn mv $ board shogi
    Just hands' =
      removeFromHands currentTurn kind $
      foldr (addToHands currentTurn) (hands shogi) kinds

doMove :: Move -> Shogi -> Maybe Shogi
doMove move@Move {} shogi = do
  guard $ isLegalMove (board shogi) move
  return $ unsafeDoMove move shogi
doMove move@(Put _ kind) shogi = do
  guard $ isLegalMove (board shogi) move
  _ <- removeFromHands (turn shogi) kind (hands shogi)
  return $ unsafeDoMove move shogi
{-
parseCsa :: String -> Shogi
parseCsa strs
-}
