module Board.Const where

import Board (Board(..), initialArray)
import Board.AbilityProxy (normalAbilityProxy)
import Board.Effector (normalEffector)
import Board.Mover (normalMover)
import Board.MoverPredicator (normalMoverPredicator)
import Board.Slicer (normalSlicer)

initialBoard :: Board
initialBoard =
  Board
    { getMover = normalMover
    , getEffector = normalEffector
    , getAbilityProxy = normalAbilityProxy
    , getSlicer = normalSlicer
    , getMoverPredicator = normalMoverPredicator
    , size = (9, 9)
    , vector = initialArray
    }
