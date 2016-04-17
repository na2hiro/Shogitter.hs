module Search.MaterialAlphaBeta(alphaBeta, iterativeDeepeningAlphaBeta, cachify, children, node_value) where

import Shogi(getNext, Shogi(..), Result(..), History(..), Diff(..), DetailedMove(..), judge)
import Board --(cells)
import Hands(toList)
import Piece(Kind(..), Piece(..), Promoted)
import Color(Color(..))
import Data.Tree.Game_tree.Game_tree(Game_tree(..))
import Data.Tree.Game_tree.Negascout(negascout)

alphaBeta :: Game_tree g => g -> Int -> ([g], Int)
alphaBeta = negascout

instance Game_tree Shogi where
    is_terminal shogi = case judge shogi of
        Just Win -> True
        _ -> False
    node_value = evaluate
    children shogi = if is_terminal shogi
        then []
        else filter notLose$ getNext shogi -- Exclude lose move
        where notLose s | Just Lose <- judge s = False
              notLose _ = True

data CachedShogi = CachedShogi Int Shogi deriving Show

instance Game_tree CachedShogi where
    is_terminal (CachedShogi _ shogi) = is_terminal shogi
    node_value (CachedShogi eval _) = eval
    children (CachedShogi eval shogi) = map cachify'$ children shogi
        where cachify' shogi = case history shogi of
                History [] -> cachify shogi
                History (diff:_) -> CachedShogi newEval shogi
                  where newEval = if is_terminal shogi then -evalInfinity else evaluateDiff eval diff

evaluateDiff lastEval (Diff (DMove color from to kind promoted captured)) = -valPromote-valCapture-lastEval
    where valPromote = if promoted then value kind True - value kind False else 0
          valCapture = case captured of
            Just (kind', promoted') -> value kind' False + value kind' promoted'
            Nothing -> 0
evaluateDiff lastEval _ = -lastEval

cachify :: Shogi -> CachedShogi
cachify shogi = CachedShogi (evaluate shogi) shogi

iterativeDeepeningAlphaBeta :: Game_tree g => g -> [([g], Int)]
iterativeDeepeningAlphaBeta s = map (negascout s) [1..]

evalInfinity :: Int
evalInfinity = maxBound-4

evaluate :: Shogi -> Int
evaluate shogi = if is_terminal shogi
    then -evalInfinity -- lose
    else (if turn shogi==Black then 1 else -1) * (handValue Black - handValue White + boardValue)
    where handValue :: Color -> Int
          handValue color = sum$ map (\(kind, num)->value kind False*num)$ toList color$ hands shogi
          boardValue = sum$ do
              (_, piece) <- cells$ board shogi
              case piece of
                  Just (Piece Black promoted kind) -> return$ value kind promoted
                  Just (Piece White promoted kind) -> return$ -value kind promoted
                  _ -> return 0

value :: Kind -> Promoted -> Int
value FU False = 87
value KY False = 232
value KE False = 257
value GI False = 369
value KI False = 534
value KA False = 489
value HI False = 510
value FU True = 495
value KY True = 444
value KE True = 569
value GI True = 642
value KA True = 827
value HI True = 945
value OU False = 9999
