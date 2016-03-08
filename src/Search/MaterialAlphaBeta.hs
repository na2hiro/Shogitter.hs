module Search.MaterialAlphaBeta(alphaBeta) where

import Shogi(getNext, Shogi(..))
import Board(cells)
import Hands(toList)
import Piece(Kind(..), Piece(..), Promoted)
import Color(Color(..))
import Data.Tree.Game_tree.Game_tree(Game_tree(..))
import Data.Tree.Game_tree.Negascout(negascout)

alphaBeta :: Shogi m e a s mp -> Int -> ([Shogi m e a s mp], Int)
alphaBeta = negascout

instance Game_tree (Shogi m e a s mp) where
    is_terminal _ = False
    node_value = evaluate
    children = getNext

evaluate :: Shogi m e a s mp -> Int
evaluate shogi@(Shogi turn board hands) = (if turn==Black then 1 else -1)* (handValue Black - handValue White + boardValue)
    where handValue :: Color -> Int
          handValue color = sum$ map (\(kind, num)->value kind False*num)$ toList color hands
          boardValue = sum$ do
              (c, piece) <- cells board
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
