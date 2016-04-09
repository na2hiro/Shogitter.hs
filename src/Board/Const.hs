module Board.Const where

import Board(Board(..), initialArray)
import Board.AbilityProxy(normalAbilityProxy)
import Board.Slicer(normalSlicer)
import Board.Mover(normalMover)
import Board.Effector(normalEffector)
import Board.MoverPredicator(normalMoverPredicator)

initialBoard :: Board
initialBoard = Board {
    getMover = normalMover,
    getEffector = normalEffector,
    getAbilityProxy = normalAbilityProxy,
    getSlicer = normalSlicer,
    getMoverPredicator = normalMoverPredicator,

    size = (9,9),
    vector = initialArray
}
