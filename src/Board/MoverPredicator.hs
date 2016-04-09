module Board.MoverPredicator
    ( normalMoverPredicator
    , freezeMoverPredicator
    , madrasMoverPredicator
    ) where

import Piece(Piece(..))
import Board

normalMoverPredicator = MoverPredicator {
    runMoverPredicator = canMove
} where
    canMove _ _ = True

freezeMoverPredicator = MoverPredicator {
    runMoverPredicator = canMove
} where
    canMove board (coord, Piece turn _ _) = coord `notElem` enemiesDestinations
        where enemies = map fst$ filter (isEnemy. snd)$ cells board
              isEnemy (Just (Piece color _ _)) | turn/=color = True
              isEnemy _ = False
              enemiesDestinations = concatMap (destinationsAt board) enemies

madrasMoverPredicator = MoverPredicator {
    runMoverPredicator = canMove
} where
    canMove board (coord, Piece turn promoted kind) = coord `notElem` enemiesDestinations
        where enemies = map fst$ filter (isMadrasEnemy. snd)$ cells board
              isMadrasEnemy (Just (Piece color promoted' kind')) | turn/=color && promoted==promoted' && kind==kind' = True
              isMadrasEnemy _ = False
              enemiesDestinations = concatMap (destinationsAt board) enemies
