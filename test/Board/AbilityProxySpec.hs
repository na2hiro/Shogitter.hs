module Board.AbilityProxySpec where

import Test.Hspec
import Board(Board(..), destinationsAt, sets, initialBoard)
import Board.AbilityProxy
import Board.Slicer
import Board.Mover(NormalMover)
import Board.Effector(NormalEffector)
import Coord(Coord(..))
import Color(Color(..))
import Piece(Piece(..), Kind(..))

type NBoard a = Board NormalMover NormalEffector a NormalSlicer

spec :: Spec
spec = do
    describe "Annan destinationsAt"$
        let diffs = [(Coord 2 1, Just$ Piece White True KE), (Coord 2 9, Just$ Piece White True KE)]
            b = sets initialBoard diffs :: NBoard AnnanAbilityProxy in do
        it "(7,7)"$ destinationsAt b (Coord 7 7) `shouldMatchList` [Coord 7 6]
        it "(2,7)"$ destinationsAt b (Coord 2 7) `shouldMatchList` [Coord 2 6, Coord 2 5, Coord 2 4, Coord 2 3]
        it "(7,9)"$ destinationsAt b (Coord 7 9) `shouldMatchList` [Coord 7 8, Coord 6 8]
        it "(8,9)"$ destinationsAt b (Coord 8 9) `shouldMatchList` []
        it "(8,2)"$ destinationsAt b (Coord 8 2) `shouldMatchList` [Coord 9 4, Coord 7 4]
        it "(2,2)"$ destinationsAt b (Coord 2 2) `shouldMatchList` [Coord 1 2, Coord 3 2]
        it "(2,8)"$ destinationsAt b (Coord 2 8) `shouldMatchList`
            [Coord 2 9, Coord 1 8, Coord 3 8, Coord 4 8, Coord 5 8, Coord 6 8, Coord 7 8]

    describe "Anhoku destinationsAt"$ let b = initialBoard :: NBoard AnhokuAbilityProxy in do
        it "(7,7)"$ destinationsAt b (Coord 7 7) `shouldMatchList` [Coord 7 6]
        it "(2,7)"$ destinationsAt b (Coord 2 7) `shouldMatchList` [Coord 2 6]
        it "(7,9)"$ destinationsAt b (Coord 7 9) `shouldMatchList` [Coord 7 8, Coord 6 8]
        it "(8,9)"$ destinationsAt b (Coord 8 9) `shouldMatchList` [Coord 9 8, Coord 7 8]
        it "(8,2)"$ destinationsAt b (Coord 8 2) `shouldMatchList` []
        it "(2,2)"$ destinationsAt b (Coord 2 2) `shouldMatchList` []

    describe "Antouzai destinationsAt"$
        let diffs = [(Coord 7 1, Nothing), (Coord 7 2, Just$ Piece White False GI), (Coord 6 9, Just$ Piece White False KI)]
            b = sets initialBoard diffs :: NBoard AntouzaiAbilityProxy in do
        it "(7,7)"$ destinationsAt b (Coord 7 7) `shouldMatchList` [Coord 7 6]
        it "(2,7)"$ destinationsAt b (Coord 2 7) `shouldMatchList` [Coord 2 6]
        it "(7,9)"$ destinationsAt b (Coord 7 9) `shouldMatchList` []
        it "(8,9)"$ destinationsAt b (Coord 8 9) `shouldMatchList` [Coord 9 8, Coord 7 8]
        it "(8,2)"$ destinationsAt b (Coord 8 2) `shouldMatchList` [Coord 7 1]
        it "(7,2)"$ destinationsAt b (Coord 7 2) `shouldMatchList`
            [Coord 7 1, Coord 6 2, Coord 5 2, Coord 4 2, Coord 3 2]
        it "(2,2)"$ destinationsAt b (Coord 2 2) `shouldMatchList` []

    describe "Anki destinationsAt"$
        let diffs = [(Coord 7 1, Nothing), (Coord 7 2, Just$ Piece White False GI), (Coord 6 1, Just$ Piece Black True GI),
                     (Coord 4 7, Just$ Piece White False FU), (Coord 4 9, Just$ Piece White False KI)]
            b = sets initialBoard diffs :: NBoard AnkiAbilityProxy in do
        it "(7,7)"$ destinationsAt b (Coord 7 7) `shouldMatchList`
            [Coord 8 6, Coord 7 6, Coord 6 6, Coord 7 8, Coord 8 5, Coord 6 5]
        it "(2,7)"$ destinationsAt b (Coord 2 7) `shouldMatchList`
            [Coord 2 6, Coord 2 5, Coord 2 4, Coord 2 3, Coord 3 6, Coord 1 6, Coord 1 8, Coord 3 8]
        it "(7,9)"$ destinationsAt b (Coord 7 9) `shouldMatchList` [Coord 7 8]
        it "(8,9)"$ destinationsAt b (Coord 8 9) `shouldMatchList` []
        it "(8,2)"$ destinationsAt b (Coord 8 2) `shouldMatchList` []
        it "(2,2)"$ destinationsAt b (Coord 2 2) `shouldMatchList` [Coord 1 2, Coord 3 2]
        it "(2,8)"$ destinationsAt b (Coord 2 8) `shouldMatchList`
            [Coord 1 8, Coord 3 8, Coord 4 8, Coord 5 8, Coord 6 8, Coord 7 8]

    describe "Tenjiku destinationsAt"$
        let diffs = [(Coord 2 9, Just$ Piece White False KE)]
            b = sets initialBoard diffs :: NBoard TenjikuAbilityProxy in do
        it "(7,7)"$ destinationsAt b (Coord 7 7) `shouldMatchList` [Coord 7 6]
        it "(2,7)"$ destinationsAt b (Coord 2 7) `shouldMatchList` [Coord 2 6, Coord 2 5, Coord 2 4, Coord 2 3]
        it "(7,9)"$ destinationsAt b (Coord 7 9) `shouldMatchList` [Coord 7 8, Coord 6 8]
        it "(8,9)"$ destinationsAt b (Coord 8 9) `shouldMatchList` []
        it "(8,2)"$ destinationsAt b (Coord 8 2) `shouldMatchList`
            [Coord 9 2, Coord 7 2, Coord 6 2, Coord 5 2, Coord 4 2, Coord 3 2, Coord 9 4, Coord 7 4]
        it "(2,2)"$ destinationsAt b (Coord 2 2) `shouldMatchList` [Coord 1 4, Coord 3 4]
        it "(2,8)"$ destinationsAt b (Coord 2 8) `shouldMatchList`
            [Coord 2 9, Coord 1 8, Coord 3 8, Coord 4 8, Coord 5 8, Coord 6 8, Coord 7 8]

    describe "Nekosen destinationsAt"$
        let diffs = [(Coord 8 3, Just$ Piece Black False FU)]
            b = sets initialBoard diffs :: NBoard NekosenAbilityProxy in do
        it "(7,7)"$ destinationsAt b (Coord 7 7) `shouldMatchList` [Coord 7 6]
        it "(2,7)"$ destinationsAt b (Coord 2 7) `shouldMatchList` [Coord 3 5, Coord 1 5]
        it "(7,9)"$ destinationsAt b (Coord 7 9) `shouldMatchList` [Coord 7 8, Coord 6 8]
        it "(8,9)"$ destinationsAt b (Coord 8 9) `shouldMatchList` []
        it "(8,2)"$ destinationsAt b (Coord 8 2) `shouldMatchList` [Coord 9 4, Coord 7 4]
        it "(2,2)"$ destinationsAt b (Coord 2 2) `shouldMatchList` []

    describe "Nekonekosen destinationsAt"$
        let diffs = [(Coord 8 3, Just$ Piece Black False FU)]
            b = sets initialBoard diffs :: NBoard NekonekosenAbilityProxy in do
        it "(7,7)"$ destinationsAt b (Coord 7 7) `shouldMatchList` [Coord 7 6]
        it "(2,7)"$ destinationsAt b (Coord 2 7) `shouldMatchList` [Coord 3 5, Coord 1 5]
        it "(7,9)"$ destinationsAt b (Coord 7 9) `shouldMatchList` [Coord 7 8, Coord 6 8]
        it "(8,9)"$ destinationsAt b (Coord 8 9) `shouldMatchList` []
        it "(8,2)"$ destinationsAt b (Coord 8 2) `shouldMatchList`
            [Coord 8 3, Coord 9 2, Coord 7 2, Coord 6 2, Coord 5 2, Coord 4 2, Coord 3 2]
        it "(8,3)"$ destinationsAt b (Coord 8 3) `shouldMatchList` [Coord 9 1, Coord 7 1]

    describe "YokoNekosen destinationsAt"$
        let diffs = [(Coord 1 6, Just$ Piece Black False FU),
                     (Coord 1 7, Just$ Piece Black False KY),
                     (Coord 1 9, Nothing),
                     (Coord 4 1, Just$ Piece Black False KI)]
            b = sets initialBoard diffs :: NBoard YokoNekosenAbilityProxy in do
        it "(7,7)"$ destinationsAt b (Coord 7 7) `shouldMatchList` [Coord 7 6]
        it "(9,7)"$ destinationsAt b (Coord 9 7) `shouldMatchList` [Coord 9 6, Coord 9 5, Coord 9 4, Coord 9 3]
        it "(7,9)"$ destinationsAt b (Coord 7 9) `shouldMatchList` [Coord 7 8, Coord 6 8]
        it "(8,9)"$ destinationsAt b (Coord 8 9) `shouldMatchList` [Coord 9 8, Coord 7 8]
        it "(5,1)"$ destinationsAt b (Coord 5 1) `shouldMatchList` [Coord 5 2]
        it "(6,1)"$ destinationsAt b (Coord 6 1) `shouldMatchList` []

    describe "YokoNekonekosen destinationsAt"$
        let diffs = [(Coord 1 6, Just$ Piece Black False FU),
                     (Coord 1 7, Just$ Piece Black False KY),
                     (Coord 1 9, Nothing),
                     (Coord 4 1, Just$ Piece Black False KI)]
            b = sets initialBoard diffs :: NBoard YokoNekonekosenAbilityProxy in do
        it "(7,7)"$ destinationsAt b (Coord 7 7) `shouldMatchList` [Coord 7 6]
        it "(9,7)"$ destinationsAt b (Coord 9 7) `shouldMatchList` [Coord 9 6, Coord 9 5, Coord 9 4, Coord 9 3]
        it "(7,9)"$ destinationsAt b (Coord 7 9) `shouldMatchList` [Coord 7 8, Coord 6 8]
        it "(8,9)"$ destinationsAt b (Coord 8 9) `shouldMatchList` [Coord 9 8, Coord 7 8]
        it "(5,1)"$ destinationsAt b (Coord 5 1) `shouldMatchList` [Coord 4 1, Coord 6 2, Coord 5 2, Coord 4 2]
        it "(6,1)"$ destinationsAt b (Coord 6 1) `shouldMatchList` [Coord 7 2, Coord 6 2, Coord 5 2]

    describe "Taimen destinationsAt"$
        let diffs = [(Coord 8 8, Just$ Piece White False KA),
                     (Coord 2 7, Just$ Piece White False FU)]
            b = sets initialBoard diffs :: NBoard TaimenAbilityProxy in do
        it "(8,7)"$ destinationsAt b (Coord 8 7) `shouldMatchList` [Coord 8 6]
        it "(8,8)"$ destinationsAt b (Coord 8 8) `shouldMatchList` []
        it "(8,9)"$ destinationsAt b (Coord 8 9) `shouldMatchList` [Coord 7 8, Coord 9 8]
        it "(2,7)"$ destinationsAt b (Coord 2 7) `shouldMatchList`
            [Coord 1 7, Coord 3 7, Coord 2 8, Coord 2 6, Coord 2 5, Coord 2 4]
        it "(2,8)"$ destinationsAt b (Coord 2 8) `shouldMatchList` [Coord 2 7]
        it "(2,9)"$ destinationsAt b (Coord 2 9) `shouldMatchList` []

    describe "Haimen destinationsAt"$
        let diffs = [(Coord 8 9, Just$ Piece White False KE),
                     (Coord 2 8, Just$ Piece White False HI)]
            b = sets initialBoard diffs :: NBoard HaimenAbilityProxy in do
        it "(8,7)"$ destinationsAt b (Coord 8 7) `shouldMatchList` [Coord 8 6]
        it "(8,8)"$ destinationsAt b (Coord 8 8) `shouldMatchList` [Coord 7 6, Coord 9 6]
        it "(8,9)"$ destinationsAt b (Coord 8 9) `shouldMatchList` [Coord 7 8, Coord 6 7, Coord 9 8]
        it "(2,7)"$ destinationsAt b (Coord 2 7) `shouldMatchList`
            [Coord 2 8, Coord 2 6, Coord 2 5, Coord 2 4, Coord 2 3]
        it "(2,8)"$ destinationsAt b (Coord 2 8) `shouldMatchList` [Coord 2 9]
        it "(2,9)"$ destinationsAt b (Coord 2 9) `shouldMatchList` []
