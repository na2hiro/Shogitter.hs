module Board.Mover
  ( normalMover
  , movers
  , getMoverById
  ) where

import Data.Maybe (maybeToList)
import Board
import Piece (Piece(..), promote)

movers :: [Mover]
movers = [normalMover]

normalMover = Mover {runMover = move, moverId = "normal"}
  where
    move _ (Move from to promoted) board = (effect from to board', kinds)
      where
        Just fromPiece = get board from
        toPiece = Just $ promote promoted fromPiece
        kinds = map getKind $ maybeToList $ get board to
        getKind (Piece _ _ kind) = kind
        board' = sets board [(from, Nothing), (to, toPiece)]
    move turn (Put to kind) board = (effectPut to board', [])
      where
        board' = set board (to, Just $ Piece turn False kind)

getMoverById id = head (filter (\ap -> moverId ap == id) movers)
