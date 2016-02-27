module Shogi.MoverPredicatorSpec where

import Test.Hspec
import Shogi
import Board(Move(..))
import Coord(Coord(..))
import Board.Slicer(NormalSlicer)
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Mover(NormalMover)
import Board.Effector(NormalEffector)
import Shogi.MoverPredicator

type NShogi mp = Shogi NormalMover NormalEffector NormalAbilityProxy NormalSlicer mp

spec :: Spec
spec = do
    describe "Freeze canMove"$
        let s = unsafeDoMove (Move (Coord 7 7) (Coord 7 6) False) initialShogi :: NShogi FreezeMoverPredicator in do
        it "(3,3)"$ canMoveCoord s (Coord 3 3) `shouldBe` False
        it "(4,3)"$ canMoveCoord s (Coord 4 3) `shouldBe` True
        let s' = unsafeDoMove (Move (Coord 4 3) (Coord 4 4) False) s
            s'' = unsafeDoMove (Move (Coord 8 8) (Coord 4 4) False) s'
            s''' = unsafeDoMove (Move (Coord 8 2) (Coord 4 2) False) s'' in do
                it "(4,4)"$ canMoveCoord s''' (Coord 4 4) `shouldBe` False
                it "(4,7)"$ canMoveCoord s''' (Coord 4 7) `shouldBe` True

    describe "Madras canMove"$
        let s = unsafeDoMove (Move (Coord 7 7) (Coord 7 6) False) initialShogi :: NShogi MadrasMoverPredicator
            s2 = unsafeDoMove (Move (Coord 3 3) (Coord 3 4) False) s in do
        it "(3,3)"$ canMoveCoord s (Coord 3 3) `shouldBe` True
        it "(4,3)"$ canMoveCoord s (Coord 4 3) `shouldBe` True
        it "(8,8)"$ canMoveCoord s2 (Coord 8 8) `shouldBe` False
        let s' = unsafeDoMove (Move (Coord 7 3) (Coord 7 4) False) s
            s'' = unsafeDoMove (Move (Coord 8 8) (Coord 3 3) False) s'
            s2'' = unsafeDoMove (Move (Coord 8 8) (Coord 3 3) True) s' in do
                it "(2,2)"$ canMoveCoord s'' (Coord 2 2) `shouldBe` False
                it "(2,2) Promoted"$ canMoveCoord s2'' (Coord 2 2) `shouldBe` True
