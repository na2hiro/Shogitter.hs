module Board.MoverPredicatorSpec where

import Test.Hspec
import Piece(promote)
import Shogi(Shogi(..))
import Board
import Coord(Coord(..))
import Color(Color(..))
import Hands(initialHands)
import Board.Slicer(NormalSlicer)
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Mover(NormalMover)
import Board.Effector(NormalEffector)
import Board.MoverPredicator
import Debug.Trace(trace)

type NBoard mp = Board NormalMover NormalEffector NormalAbilityProxy NormalSlicer mp

pseudoDoMove :: Move -> Board m e a s mp -> Board m e a s mp
pseudoDoMove (Move from to promoted) board = let board' = sets board [(from, Nothing), (to, Just$ promote promoted piece)] in trace (show board') board'
    where piece = unsafeGet board from

spec :: Spec
spec = do
    describe "Freeze canMove"$
        let s = pseudoDoMove (Move (Coord 7 7) (Coord 7 6) False) initialBoard :: NBoard FreezeMoverPredicator in do
        it "(3,3)"$ canMoveCoord s (Coord 3 3) `shouldBe` False
        it "(3,3) Shogi"$ getMovesFrom s (Coord 3 3) `shouldBe` []
        it "(4,3)"$ canMoveCoord s (Coord 4 3) `shouldBe` True
        it "(4,3) Shogi"$ getMovesFrom s (Coord 4 3) `shouldNotBe` []
        let s' = pseudoDoMove (Move (Coord 4 3) (Coord 4 4) False) s
            s'' = pseudoDoMove (Move (Coord 8 8) (Coord 4 4) False) s'
            s''' = pseudoDoMove (Move (Coord 8 2) (Coord 4 2) False) s'' in do
                it "(4,4)"$ canMoveCoord s''' (Coord 4 4) `shouldBe` False
                it "(4,4) Shogi"$ getMovesFrom s''' (Coord 4 4) `shouldBe` []
                it "(4,7)"$ canMoveCoord s''' (Coord 4 7) `shouldBe` True
                it "(4,7) Shogi"$ getMovesFrom s''' (Coord 4 7) `shouldNotBe` []

    describe "Madras canMove"$
        let s = pseudoDoMove (Move (Coord 7 7) (Coord 7 6) False) initialBoard :: NBoard MadrasMoverPredicator
            s2 = pseudoDoMove (Move (Coord 3 3) (Coord 3 4) False) s in do
        it "(3,3)"$ canMoveCoord s (Coord 3 3) `shouldBe` True
        it "(3,3) Shogi"$ getMovesFrom s (Coord 3 3) `shouldNotBe` []
        it "(4,3)"$ canMoveCoord s (Coord 4 3) `shouldBe` True
        it "(4,3) Shogi"$ getMovesFrom s (Coord 4 3) `shouldNotBe` []
        it "(8,8)"$ canMoveCoord s2 (Coord 8 8) `shouldBe` False
        it "(8,8) Shogi"$ getMovesFrom s2 (Coord 8 8) `shouldBe` []
        let s' = pseudoDoMove (Move (Coord 7 3) (Coord 7 4) False) s
            s'' = pseudoDoMove (Move (Coord 8 8) (Coord 3 3) False) s'
            s2'' = pseudoDoMove (Move (Coord 8 8) (Coord 3 3) True) s' in do
                it "(2,2)"$ canMoveCoord s'' (Coord 2 2) `shouldBe` False
                it "(2,2) Shogi"$ getMovesFrom s'' (Coord 2 2) `shouldBe` []
                it "(2,2) Promoted"$ canMoveCoord s2'' (Coord 2 2) `shouldBe` True
                it "(2,2) Shogi"$ getMovesFrom s2'' (Coord 2 2) `shouldNotBe` []
