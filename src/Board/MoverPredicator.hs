module Board.MoverPredicator
    ( NormalMoverPredicator
    , FreezeMoverPredicator
    , MadrasMoverPredicator
    ) where

import Piece(Piece(..))
import {-# SOURCE #-}Board

data NormalMoverPredicator
instance MoverPredicator NormalMoverPredicator where
    canMove _ _ = True

data FreezeMoverPredicator
instance MoverPredicator FreezeMoverPredicator where
    canMove board (coord, Piece turn _ _) = coord `notElem` enemiesDestinations
        where enemies = map fst$ filter (isEnemy. snd)$ cells board
              isEnemy (Just (Piece color _ _)) | turn/=color = True
              isEnemy _ = False
              enemiesDestinations = concatMap (destinationsAt board) enemies

data MadrasMoverPredicator
instance MoverPredicator MadrasMoverPredicator where
    canMove board (coord, Piece turn promoted kind) = coord `notElem` enemiesDestinations
        where enemies = map fst$ filter (isMadrasEnemy. snd)$ cells board
              isMadrasEnemy (Just (Piece color promoted' kind')) | turn/=color && promoted==promoted' && kind==kind' = True
              isMadrasEnemy _ = False
              enemiesDestinations = concatMap (destinationsAt board) enemies
