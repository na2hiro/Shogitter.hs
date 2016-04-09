module BoardSpec where

import Test.Hspec
import Board
import Board.Const(initialBoard)
import Coord(Coord(..))
import Piece(Piece(..), Kind(..))
import Color(Color(..))

initial :: Board
initial = initialBoard

spec :: Spec
spec = do
    describe "get"$ do
        it "(7,7)"$ get initial (Coord 7 7) `shouldBe` Just (Piece Black False FU)
        it "(7,6)"$ get initial (Coord 7 6) `shouldBe` Nothing
        it "(2,1)"$ get initial (Coord 2 1) `shouldBe` Just (Piece White False KE)
    describe "set"$ do
        it "+0076FU"$ let next = set initial (Coord 7 6, Just$ Piece Black False FU)
            in get next (Coord 7 6) `shouldBe` Just (Piece Black False FU)
        it "clear 77"$ let next = set initial (Coord 7 7, Nothing)
            in get next (Coord 7 7) `shouldBe` Nothing
        it "+0079GI"$ set initial (Coord 7 9, Just$ Piece Black False GI) `shouldBe` initial
        it "clear 98"$ set initial (Coord 9 8, Nothing) `shouldBe` initial
    describe "destinationsAt"$ do
        it "(7,7)"$ destinationsAt initial (Coord 7 7) `shouldMatchList` [Coord 7 6]
        it "(7,9)"$ destinationsAt initial (Coord 7 9) `shouldMatchList` [Coord 7 8, Coord 6 8]
        it "(8,9)"$ destinationsAt initial (Coord 8 9) `shouldMatchList` []
        it "(8,2)"$ destinationsAt initial (Coord 8 2) `shouldMatchList`
            [Coord 9 2, Coord 7 2, Coord 6 2, Coord 5 2, Coord 4 2, Coord 3 2]
        it "(2,2)"$ destinationsAt initial (Coord 2 2) `shouldMatchList` []

