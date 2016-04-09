module Shogi.Const where

import Shogi(Shogi(..), initialHistory)
import Shogi.Judge(normalJudge)
import Piece(Color(..))
import Board.Const(initialBoard)
import Hands(initialHands)

initialShogi = Shogi {
    getJudge = normalJudge,

    history = initialHistory,
    turn = Black,
    board = initialBoard,
    hands = initialHands
}
