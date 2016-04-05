module Search.MaterialAlphaBeta(alphaBeta, iterativeDeepeningAlphaBeta) where

import Shogi(getNext, Shogi(..), Result(..), Judge(..))
import Board(cells)
import Hands(toList)
import Piece(Kind(..), Piece(..), Promoted)
import Color(Color(..))
import Data.Tree.Game_tree.Game_tree(Game_tree(..))
import Data.Tree.Game_tree.Negascout(negascout)

alphaBeta :: Judge j => Shogi m e a s mp j -> Int -> ([Shogi m e a s mp j], Int)
alphaBeta = negascout

instance Judge j => Game_tree (Shogi m e a s mp j) where
    is_terminal shogi = case judge shogi of
        Just Win -> True
        _ -> False
    node_value = evaluate
    children shogi = if is_terminal shogi
        then []
        else filter notLose$ getNext shogi -- Exclude lose move
        where notLose s | Just Lose <- judge s = False
              notLose _ = True

iterativeDeepeningAlphaBeta :: Judge j => Shogi m e a s mp j -> [([Shogi m e a s mp j], Int)]
iterativeDeepeningAlphaBeta s = map (alphaBeta s) [1..]

evaluate :: Judge j => Shogi m e a s mp j -> Int
evaluate shogi@(Shogi turn board hands) = if is_terminal shogi
    then minBound+4 -- lose
    else (if turn==Black then 1 else -1) * (handValue Black - handValue White + boardValue)
    where handValue :: Color -> Int
          handValue color = sum$ map (\(kind, num)->value kind False*num)$ toList color hands
          boardValue = sum$ do
              (_, piece) <- cells board
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
