module Board.EffectorSpec where

import Test.Hspec
import Board(Move(..), Effector, effect, sets, initialBoard, Board(..))
import Coord(Coord(..))
import Color(Color(..))
import Piece(Piece(..), Kind(..))
import Board.Slicer(NormalSlicer)
import Board.AbilityProxy(NormalAbilityProxy)
import Board.Mover(NormalMover)
import Board.Effector
import Shogi.MoverPredicator(NormalMoverPredicator)

type NBoard e = Board NormalMover e NormalAbilityProxy NormalSlicer

spec :: Spec
spec = do
    describe "Othello effect"$
        let diffs = map (\x->(Coord x 8, Just$ Piece White False GI)) [3..7]++[(Coord 1 8, Just$ Piece White False HI)]
            b = sets initialBoard diffs :: NBoard OthelloEffector
            b' = sets b$ map (\x->(Coord x 8, Just$ Piece Black False GI)) [3..7]
            b'' = sets b$ map (\x->(Coord x 8, Just$ Piece Black False GI)) [4..6]
            b2 = sets b [(Coord 1 8, Just$ Piece Black False HI)] in do
        it "(8,8)"$ effect (Coord 8 8) b `shouldBe` b'
        it "(5,9)"$ effect (Coord 5 9) b `shouldBe` b''
        it "(1,9)"$ effect (Coord 1 9) b `shouldBe` b2
        it "(1,7)"$ effect (Coord 1 7) b `shouldBe` b2
        it "(2,8)"$ effect (Coord 2 8) b `shouldBe` b'

    describe "Go effect"$
        let diffs = map (\x->(Coord x 8, Just$ Piece White False GI)) [3..7]++[(Coord 1 8, Just$ Piece White False HI)]
            b = sets initialBoard diffs :: NBoard GoEffector
            b' = sets b$  map (\x->(Coord x 8, Nothing)) [3..7]
            b'' = sets b [(Coord 1 8, Nothing)]

            b2 = sets b [(Coord 7 7, Nothing)]
            b2' = sets b2 [(Coord 1 8, Nothing)] in do

        it "(8,8)"$ effect (Coord 8 8) b `shouldBe` b'
        it "(5,9)"$ effect (Coord 5 9) b `shouldBe` b'
        it "(8,9)"$ effect (Coord 8 9) b `shouldBe` b
        it "(1,7)"$ effect (Coord 1 7) b `shouldBe` b''
        it "(2,8)"$ effect (Coord 2 8) b `shouldBe` initialBoard

        it "(8,8)"$ effect (Coord 8 8) b2 `shouldBe` b2
        it "(5,9)"$ effect (Coord 5 9) b2 `shouldBe` b2
        it "(8,9)"$ effect (Coord 8 9) b2 `shouldBe` b2
        it "(1,7)"$ effect (Coord 1 7) b2 `shouldBe` b2'
        it "(2,8)"$ effect (Coord 2 8) b2 `shouldBe` b2'

    describe "Nip effect"$
        let diffs = map (\x->(Coord x 8, Just$ Piece White False GI)) [3..7]++[(Coord 1 8, Just$ Piece White False HI)]
            b = sets initialBoard diffs :: NBoard NipEffector
            b' = sets b$ map (\x->(Coord x 8, Nothing)) [3..7]
            b'' = sets b [(Coord 1 8, Nothing)]

            b2 = sets b [(Coord 7 7, Nothing)]
            b2' = sets b2$ map (\x->(Coord x 8, Nothing)) [3..7]
            b2'' = sets b2 [(Coord 5 8, Nothing)]
            b2''' = sets b2 [(Coord 1 8, Nothing)]
            b2'''' = sets b2' [(Coord 1 8, Nothing)] in do

        it "(8,8)"$ effect (Coord 8 8) b `shouldBe` b'
        it "(5,9)"$ effect (Coord 5 9) b `shouldBe` b'
        it "(1,7)"$ effect (Coord 1 7) b `shouldBe` b''
        it "(2,8)"$ effect (Coord 2 8) b `shouldBe` initialBoard
        it "(8,9)"$ effect (Coord 8 9) b `shouldBe` b

        it "(8,8)"$ effect (Coord 8 8) b2 `shouldBe` b2'
        it "(5,9)"$ effect (Coord 5 9) b2 `shouldBe` b2''
        it "(1,7)"$ effect (Coord 1 7) b2 `shouldBe` b2'''
        it "(2,8)"$ effect (Coord 2 8) b2 `shouldBe` b2''''
        it "(8,9)"$ effect (Coord 8 9) b2 `shouldBe` b2
