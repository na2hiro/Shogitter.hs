module Board.Mover
    ( NormalMover
    ) where

import {-# SOURCE #-}Board
import Piece(Piece(..), promote)
import Data.Maybe(maybeToList)

data NormalMover
instance Mover NormalMover where
    move (Move from to promoted) board = (sets board [(from, Nothing), (to, toPiece)], kinds)
        where Just fromPiece = get board from
              toPiece = Just$ promote promoted fromPiece
              kinds = map getKind$ maybeToList$ get board to
              getKind (Piece _ _ kind) = kind
    move (Put _ _) _ = error "put is not supported for Put"
