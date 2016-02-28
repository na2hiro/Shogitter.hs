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

from :: Coord
from = undefined

spec :: Spec
spec = do
    describe "Othello effect"$
        let diffs = map (\x->(Coord x 8, Just$ Piece White False GI)) [3..7]++[(Coord 1 8, Just$ Piece White False HI)]
            b = sets initialBoard diffs :: NBoard OthelloEffector
            b' = sets b$ map (\x->(Coord x 8, Just$ Piece Black False GI)) [3..7]
            b'' = sets b$ map (\x->(Coord x 8, Just$ Piece Black False GI)) [4..6]
            b2 = sets b [(Coord 1 8, Just$ Piece Black False HI)] in do
        it "(8,8)"$ effect from (Coord 8 8) b `shouldBe` b'
        it "(5,9)"$ effect from (Coord 5 9) b `shouldBe` b''
        it "(1,9)"$ effect from (Coord 1 9) b `shouldBe` b2
        it "(1,7)"$ effect from (Coord 1 7) b `shouldBe` b2
        it "(2,8)"$ effect from (Coord 2 8) b `shouldBe` b'

    describe "Go effect"$
        let diffs = map (\x->(Coord x 8, Just$ Piece White False GI)) [3..7]++[(Coord 1 8, Just$ Piece White False HI)]
            b = sets initialBoard diffs :: NBoard GoEffector
            b' = sets b$  map (\x->(Coord x 8, Nothing)) [3..7]
            b'' = sets b [(Coord 1 8, Nothing)]

            b2 = sets b [(Coord 7 7, Nothing)]
            b2' = sets b2 [(Coord 1 8, Nothing)] in do

        it "(8,8)"$ effect from (Coord 8 8) b `shouldBe` b'
        it "(5,9)"$ effect from (Coord 5 9) b `shouldBe` b'
        it "(8,9)"$ effect from (Coord 8 9) b `shouldBe` b
        it "(1,7)"$ effect from (Coord 1 7) b `shouldBe` b''
        it "(2,8)"$ effect from (Coord 2 8) b `shouldBe` initialBoard

        it "(8,8)"$ effect from (Coord 8 8) b2 `shouldBe` b2
        it "(5,9)"$ effect from (Coord 5 9) b2 `shouldBe` b2
        it "(8,9)"$ effect from (Coord 8 9) b2 `shouldBe` b2
        it "(1,7)"$ effect from (Coord 1 7) b2 `shouldBe` b2'
        it "(2,8)"$ effect from (Coord 2 8) b2 `shouldBe` b2'

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

        it "(8,8)"$ effect from (Coord 8 8) b `shouldBe` b'
        it "(5,9)"$ effect from (Coord 5 9) b `shouldBe` b'
        it "(1,7)"$ effect from (Coord 1 7) b `shouldBe` b''
        it "(2,8)"$ effect from (Coord 2 8) b `shouldBe` initialBoard
        it "(8,9)"$ effect from (Coord 8 9) b `shouldBe` b

        it "(8,8)"$ effect from (Coord 8 8) b2 `shouldBe` b2'
        it "(5,9)"$ effect from (Coord 5 9) b2 `shouldBe` b2''
        it "(1,7)"$ effect from (Coord 1 7) b2 `shouldBe` b2'''
        it "(2,8)"$ effect from (Coord 2 8) b2 `shouldBe` b2''''
        it "(8,9)"$ effect from (Coord 8 9) b2 `shouldBe` b2

    describe "UnderWater effect"$
        let piece kind = Just$ Piece Black False kind
            fu = piece FU
            diffs = [(Coord 7 6, fu), (Coord 7 7, Nothing)]
            b = sets initialBoard diffs :: NBoard UnderWaterEffector
            b' = sets b [(Coord 7 6, Nothing)]

            b2 = sets b [(Coord 7 6, Nothing), (Coord 2 7, Nothing), (Coord 2 6, fu)]
            b2' = sets b2$ (Coord 2 6, Nothing):map (\x->(Coord x 7, Nothing)) [3..7]

            b3 = sets b2' [(Coord 2 8, Nothing), (Coord 2 7, piece HI)] in do

        it "7 6 FU"$ effect (Coord 7 7) (Coord 7 6) b `shouldBe` b'

        it "2 6 FU"$ effect (Coord 2 7) (Coord 2 6) b2 `shouldBe` b2'

        it "2 7 HI"$ effect (Coord 2 8) (Coord 2 7) b3 `shouldBe` b3

    describe "Nuclear effect"$
        let piece kind = Just$ Piece Black False kind
            fu = piece FU
            diffs = [(Coord 7 4, fu), (Coord 7 7, Nothing)]
            b = sets initialBoard diffs :: NBoard NuclearEffector
            b' = sets b [(Coord 7 3, Just$ Piece White True FU)]
            b'' = sets b [(Coord 7 4, Just$ Piece Black True FU)]
            b''' = sets b [(Coord 3 3, Just$ Piece White True FU)]

            b2 = sets b [(Coord 8 8, Nothing), (Coord 3 3, Just$ Piece Black True KA)]
            b2' = sets b2$ (Coord 2 2, Just$ Piece White True KA): map (\x->(Coord x 3, Just$ Piece White True FU)) [2,4] in do

        it "(7,4)"$ effect from (Coord 7 4) b `shouldBe` b'
        it "(7,3)"$ effect from (Coord 7 3) b `shouldBe` b''
        it "(8,8)"$ effect from (Coord 8 8) b `shouldBe` b'''

        it "(3,3)"$ effect from (Coord 3 3) b2 `shouldBe` b2'

    describe "Donden effect"$
        let piece kind = Just$ Piece Black False kind
            fu = piece FU
            diffs = [(Coord 7 2, fu), (Coord 7 7, Nothing)]
            b = sets initialBoard diffs :: NBoard DondenEffector
            b' = sets b [(Coord 7 2, Just$ Piece Black False GI), (Coord 7 1, Just$ Piece White False FU)]

            diffs2 = [(Coord 7 2, Just$ Piece Black True FU), (Coord 7 7, Nothing)]
            b2 = sets initialBoard diffs2 :: NBoard DondenEffector
            b2' = sets b2 [(Coord 7 2, Just$ Piece Black False GI), (Coord 7 1, Just$ Piece White True FU)] in do

        it "(7,2)"$ effect from (Coord 7 2) b `shouldBe` b'
        it "(7,1)"$ effect from (Coord 7 1) b `shouldBe` b'
        it "(7,3)"$ effect from (Coord 7 3) b `shouldBe` b
        it "(8,2)"$ effect from (Coord 8 2) b `shouldBe` b

        it "(7,2)"$ effect from (Coord 7 2) b2 `shouldBe` b2'
        it "(7,1)"$ effect from (Coord 7 1) b2 `shouldBe` b2'
        it "(7,3)"$ effect from (Coord 7 3) b2 `shouldBe` b2
        it "(8,2)"$ effect from (Coord 8 2) b2 `shouldBe` b2

    describe "Gravity effect"$
        let piece kind = Just$ Piece Black False kind
            fu = piece FU
            initial = effect from from initialBoard :: NBoard GravityEffector
            diffs = [(Coord 7 6, fu), (Coord 7 7, Nothing)]
            b = sets initial diffs
            b' = sets b [(Coord 7 6, Nothing), (Coord 1 6, fu), (Coord 9 7, Nothing), (Coord 7 7, fu)]

            b2 = sets initial [(Coord 2 2, Nothing), (Coord 8 2, Just$ Piece White False HI)] in do

        it "7 6 FU"$ effect from (Coord 7 6) b `shouldBe` b'
        it "8 2 HI"$ effect from (Coord 8 2) b2 `shouldBe` initial

