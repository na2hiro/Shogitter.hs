module Shogi.Judge
    ( Result(..)
    , normalJudge
    , absentJudge
    , mateJudge
    , tryJudge
    , checkMateJudge
    , othelloJudge
    , gomokuJudge
    , winHandCountJudge
    , judges
    ) where

import Prelude hiding(id)
import Shogi
import Board
import Coord
import Coord.Const(eightDirections)
import Color
import Piece(Piece(..), Kind(OU))
import Hands(toList)
import Data.List(foldl')
import Control.Monad(foldM)
import Control.Arrow((***))

judges :: [Judge]
judges =
    [ normalJudge
    , absentJudge
    , mateJudge
    , tryJudge
    , checkMateJudge
    , othelloJudge
    , gomokuJudge
    , winHandCountJudge
    ]

normalJudge = Judge {
    judgeId = "normal",
    runJudge = judge
} where
    judge _ = Nothing

absentJudge = Judge {
    judgeId = "absent",
    runJudge = judge
} where
    judge shogi
        | Nothing <- ouTurn = Just Lose
        | Nothing <- ouNextTurn = Just Win
        | otherwise = Nothing
        where nextTurn = turn shogi
              (ouTurn, ouNextTurn) = orderTuple currentTurn$ getOu$ board shogi
              currentTurn = opposite nextTurn

getOu :: Board -> (Maybe (Coord, Cell), Maybe (Coord, Cell))
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

mateJudge = Judge {
    judgeId = "mate",
    runJudge = judge
} where
    judge shogi
        | mateTurn = Just Lose
        | mateNextTurn = Just Win
        | otherwise = Nothing
        where nextTurn = turn shogi
              currentTurn = opposite nextTurn
              (mateTurn, mateNextTurn) = orderTuple currentTurn$ mate shogi

-- Mate to each color's OU
-- Precondition: there's 1 OU for each side
mate :: Shogi -> (Bool, Bool)
mate shogi = (coordBlack `elem` tosWhite, coordWhite `elem` tosBlack)
        where b = board shogi
              (tosBlack, tosWhite) = getTo *** getTo$ getMovesEach b ([], []) -- No Put can pick OU
              getTo = map (\(Move _ to _)->to)
              (Just (coordBlack, _), Just (coordWhite, _)) = getOu b

-- Note: No support for multiple OU
checkMateJudge = Judge {
    judgeId = "checkMate",
    runJudge = judge
} where
    judge shogi
        | mateTurn = Just Lose
        | mateNextTurn && all (pickTuple nextTurn. mate) (getNext shogi) = Just Win
        | otherwise = Nothing
        where nextTurn = turn shogi
              currentTurn = opposite nextTurn
              (mateTurn, mateNextTurn) = orderTuple currentTurn$ mate shogi

-- TODO: Need to be combined with other rules. Error after OU is caught
tryJudge = Judge {
    judgeId = "try",
    runJudge = judge
} where
    judge shogi
        | tryNextTurn = Just Lose
        | tryTurn = Just Win
        | otherwise = Nothing
        where nextTurn = turn shogi
              currentTurn = opposite nextTurn
              (Just (coordBlack, _), Just (coordWhite, _)) = getOu$ board shogi
              (tryTurn, tryNextTurn) = orderTuple currentTurn (coordBlack == Coord 5 1, coordWhite == Coord 5 9)

othelloJudge = Judge {
    judgeId = "othello",
    runJudge = judge
} where
    judge shogi = compare. orderTuple currentTurn <$> countM (board shogi)
        where nextTurn = turn shogi
              currentTurn = opposite nextTurn
              compare (numTurn, numNextTurn) | numTurn>numNextTurn = Win
                                             | numTurn<numNextTurn = Lose
                                             | otherwise = Even

countM :: Board -> Maybe (Int, Int)
countM board = foldM f (0, 0)$ cells board
    where f acc (_, Nothing) = Nothing
          f (b, w) (_, Just (Piece Black _ _)) = Just (b+1, w)
          f (b, w) _ = Just (b, w+1)

gomokuJudge = Judge {
    judgeId = "gomoku",
    runJudge = judge
} where
    judge shogi | winNextTurn = Just Lose
                | winTurn = Just Win
                | otherwise = Nothing
        where nextTurn = turn shogi
              currentTurn = opposite nextTurn
              winNextTurn = any (fiveStreak. map (isColor nextTurn)) sliceAll
              winTurn = any (fiveStreak. map (isColor currentTurn)) sliceAll
              sliceAll :: [[(Coord, Cell)]]
              sliceAll = do
                 x <- [1..9]
                 y <- [1..9]
                 vec <- eightDirections
                 return$ sliceFinite (board shogi) (Coord x y) vec
              isColor :: Color -> (Coord, Cell) -> Bool
              isColor color (_, Just (Piece color' _ _)) | color==color' = True
              isColor _ _ = False


fiveStreak :: [Bool] -> Bool
fiveStreak = f 0
    where f n _ | n>=5 = True
          f n [] = False
          f n (True:xs) = f (n+1) xs
          f n (False:xs) = f 0 xs

winHandCountJudge = Judge {
    judgeId = "winHandCount",
    runJudge = judge
} where
    judge shogi
        | win nextTurn = Just Lose
        | win currentTurn = Just Win
        | otherwise = Nothing
        where nextTurn = turn shogi
              currentTurn = opposite nextTurn
              count hand = foldl' (+) 0$ map snd hand
              win t = count (toList t$ hands shogi) >= 3

-- TODO: It's meaningless unless this is combined with other rules
loseHandCountJudge = Judge {
    judgeId = "loseHandCount",
    runJudge = judge
} where
    judge = undefined
