module Board.SlicerSpec where

import Test.Hspec
import Board(destinationsAt, initialBoard, sets, Board)
import Piece(Piece(..), Kind(..))
import Color(Color(..))
import Coord(Coord(..))
import Board.Slicer
import Board.AbilityProxy(NormalAbilityProxy)

type NBoard s = Board NormalAbilityProxy s

spec :: Spec
spec = do
    describe "Loop destinationsAt"$
        let diffs = [(Coord 9 7, Nothing), (Coord 1 5, Just$ Piece White False GI)]
            b = sets diffs$ initialBoard :: NBoard LoopSlicer in do
        it "(8,8)"$ destinationsAt (Coord 8 8) b `shouldMatchList`
            [Coord 9 7, Coord 1 6, Coord 2 5, Coord 3 4, Coord 4 3]
        it "(1,5)"$ destinationsAt (Coord 1 5) b `shouldMatchList`
            [Coord 1 6, Coord 2 6, Coord 9 6, Coord 9 4, Coord 2 4]
        it "(7,9)"$ destinationsAt (Coord 7 9) b `shouldMatchList` [Coord 7 8, Coord 6 8]
        it "(8,9)"$ destinationsAt (Coord 8 9) b `shouldMatchList` [Coord 9 7]
        it "(8,2)"$ destinationsAt (Coord 8 2) b `shouldMatchList`
            [Coord 9 2, Coord 1 2, Coord 7 2, Coord 6 2, Coord 5 2, Coord 4 2, Coord 3 2]

    describe "Donut destinationsAt"$
        let diffs = [(Coord 9 7, Nothing), (Coord 7 9, Nothing), (Coord 1 5, Just$ Piece White False HI)]
            b = sets diffs$ initialBoard :: NBoard DonutSlicer in do
        it "(8,8)"$ destinationsAt (Coord 8 8) b `shouldMatchList`
            [Coord 9 7, Coord 1 6, Coord 2 5, Coord 3 4, Coord 4 3, Coord 7 9, Coord 6 1]
        it "(1,5)"$ destinationsAt (Coord 1 5) b `shouldMatchList`
            [Coord 1 6, Coord 1 7, Coord 1 4, Coord 2 5, Coord 3 5, Coord 4 5, Coord 5 5, Coord 6 5, Coord 7 5, Coord 8 5, Coord 9 5]
        it "(8,9)"$ destinationsAt (Coord 8 9) b `shouldMatchList` [Coord 9 7]
        it "(8,2)"$ destinationsAt (Coord 8 2) b `shouldMatchList`
            [Coord 9 2, Coord 1 2, Coord 7 2, Coord 6 2, Coord 5 2, Coord 4 2, Coord 3 2]

    describe "Reflect destinationsAt"$
        let diffs = [(Coord 9 7, Nothing), (Coord 7 9, Nothing), (Coord 1 5, Just$ Piece White False HI)]
            b = sets diffs$ initialBoard :: NBoard ReflectSlicer in do
        it "(8,8)"$ destinationsAt (Coord 8 8) b `shouldMatchList`
            [Coord 9 7, Coord 8 6, Coord 7 5, Coord 6 4, Coord 5 3, Coord 7 9, Coord 6 8]
        it "(1,5)"$ destinationsAt (Coord 1 5) b `shouldMatchList`
            [Coord 1 6, Coord 1 7, Coord 1 4, Coord 2 5, Coord 3 5, Coord 4 5, Coord 5 5, Coord 6 5, Coord 7 5, Coord 8 5, Coord 9 5]
        it "(8,9)"$ destinationsAt (Coord 8 9) b `shouldMatchList` [Coord 9 7]
        it "(8,2)"$ destinationsAt (Coord 8 2) b `shouldMatchList`
            [Coord 9 2, Coord 7 2, Coord 6 2, Coord 5 2, Coord 4 2, Coord 3 2]