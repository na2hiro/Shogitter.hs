module Board.Mover
    ( normalMover
    ) where

import Board
import Piece(Piece(..), promote)
import Data.Maybe(maybeToList)

normalMover = Mover {
    runMover = move
} where
    move _ (Move from to promoted) board = (effect from to board', kinds)
        where Just fromPiece = get board from
              toPiece = Just$ promote promoted fromPiece
              kinds = map getKind$ maybeToList$ get board to
              getKind (Piece _ _ kind) = kind
              board' = sets board [(from, Nothing), (to, toPiece)]
    move turn (Put to kind) board = (effectPut to board', [])
        where board' = set board (to, Just$ Piece turn False kind)
