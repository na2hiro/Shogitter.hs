module Shogi.MoverPredicator
    ( NormalMoverPredicator
    , FreezeMoverPredicator
    , MadrasMoverPredicator
    ) where

import {-# SOURCE #-} Shogi
import Piece(Piece(..))
import Board(cells, destinationsAt)
import Debug.Trace(trace)

data NormalMoverPredicator
instance MoverPredicator NormalMoverPredicator where
    canMove (Shogi turn _ _) (_, Piece color _ _) | color==turn = True
    canMove _ _ = False

data FreezeMoverPredicator
instance MoverPredicator FreezeMoverPredicator where
    canMove (Shogi turn board hands) (coord, _) = not$ coord `elem` enemiesDestinations
        where enemies = map fst$ filter (isEnemy. snd)$ cells board
              isEnemy (Just (Piece color _ _)) | turn/=color = True
              isEnemy _ = False
              enemiesDestinations = concatMap (flip destinationsAt board) enemies

data MadrasMoverPredicator
instance MoverPredicator MadrasMoverPredicator where
    canMove (Shogi turn board hands) (coord, Piece _ promoted kind) = not$ coord `elem` trace (show enemiesDestinations) enemiesDestinations
        where enemies = map fst$ filter (isMadrasEnemy. snd)$ cells board
              isMadrasEnemy (Just (Piece color promoted' kind')) | turn/=color && promoted==promoted' && kind==kind' = True
              isMadrasEnemy _ = False
              enemiesDestinations = concatMap (flip destinationsAt board) enemies
