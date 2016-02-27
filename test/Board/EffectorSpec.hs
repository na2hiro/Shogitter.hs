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
            b = sets diffs$ initialBoard :: NBoard OthelloEffector
            b' = sets (map (\x->(Coord x 8, Just$ Piece Black False GI)) [3..7]) b
            b'' = sets (map (\x->(Coord x 8, Just$ Piece Black False GI)) [4..6]) b
            b2 = sets [(Coord 1 8, Just$ Piece Black False HI)] b in do
        it "(8,8)"$ effect (Coord 8 8) b `shouldBe` b'
        it "(5,9)"$ effect (Coord 5 9) b `shouldBe` b''
        it "(1,9)"$ effect (Coord 1 9) b `shouldBe` b2
        it "(1,7)"$ effect (Coord 1 7) b `shouldBe` b2
        it "(2,8)"$ effect (Coord 2 8) b `shouldBe` b'

    describe "Go effect"$
        let diffs = map (\x->(Coord x 8, Just$ Piece White False GI)) [3..7]++[(Coord 1 8, Just$ Piece White False HI)]
            b = sets diffs$ initialBoard :: NBoard GoEffector
            b' = sets (map (\x->(Coord x 8, Nothing)) [3..7]) b
            b'' = sets [(Coord 1 8, Nothing)] b

            b2 = sets [(Coord 7 7, Nothing)] b
            b2' = sets [(Coord 1 8, Nothing)] b2 in do

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
            b = sets diffs$ initialBoard :: NBoard NipEffector
            b' = sets (map (\x->(Coord x 8, Nothing)) [3..7]) b
            b'' = sets [(Coord 1 8, Nothing)] b

            b2 = sets [(Coord 7 7, Nothing)] b
            b2' = sets (map (\x->(Coord x 8, Nothing)) [3..7]) b2
            b2'' = sets [(Coord 5 8, Nothing)] b2
            b2''' = sets [(Coord 1 8, Nothing)] b2
            b2'''' = sets [(Coord 1 8, Nothing)] b2' in do

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
