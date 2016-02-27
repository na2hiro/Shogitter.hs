module Board.Mover
    ( NormalMover
    ) where

import {-# SOURCE #-}Board
import Piece(Piece(..), promote)
import Data.Maybe(maybeToList)

data NormalMover
instance Mover NormalMover where
    move (Move from to promoted) board = (sets [(from, Nothing), (to, toPiece)] board, kinds)
        where Just fromPiece = get from board
              toPiece = Just$ promote promoted fromPiece
              kinds = map getKind$ maybeToList$ get to board
              getKind (Piece _ _ kind) = kind
