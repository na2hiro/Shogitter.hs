module Board.Mover
    ( NormalMover
    ) where

import {-# SOURCE #-}Board
import Piece(Piece(..), promote)
import Data.Maybe(maybeToList)

data NormalMover
instance Mover NormalMover where
    move _ (Move from to promoted) board@Board{} = (effect from to board', kinds)
        where Just fromPiece = get board from
              toPiece = Just$ promote promoted fromPiece
              kinds = map getKind$ maybeToList$ get board to
              getKind (Piece _ _ kind) = kind
              board' = sets board [(from, Nothing), (to, toPiece)]
    move turn (Put to kind) board@Board{} = (effectPut to board', [])
        where board' = set board (to, Just$ Piece turn False kind)
