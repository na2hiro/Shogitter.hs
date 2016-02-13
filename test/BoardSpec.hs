module BoardSpec where

import Test.Hspec
import Board
import Coord(Coord(..))
import Piece(Piece(..), Kind(..))
import Color(Color(..))

initial :: NormalBoard
initial = initialBoard

spec :: Spec
spec = do
    describe "get"$ do
        it "(7,7)"$ get (Coord 7 7) initial `shouldBe` Just (Piece Black False FU)
        it "(7,6)"$ get (Coord 7 6) initial `shouldBe` Nothing
        it "(2,1)"$ get (Coord 2 1) initial `shouldBe` Just (Piece White False KE)
    describe "set"$ do
        it "+0076FU"$ let next = set (Coord 7 6, Just$ Piece Black False FU) initial
            in get (Coord 7 6) next `shouldBe` Just (Piece Black False FU)
        it "clear 77"$ let next = set (Coord 7 7, Nothing) initial
            in get (Coord 7 7) next `shouldBe` Nothing
        it "+0079GI"$ set (Coord 7 9, Just$ Piece Black False GI) initial  `shouldBe` initial
        it "clear 98"$ set (Coord 9 8, Nothing) initial `shouldBe` initial
    describe "destinationsAt"$ do
        it "(7,7)"$ destinationsAt (Coord 7 7) initial `shouldMatchList` [Coord 7 6]
        it "(7,9)"$ destinationsAt (Coord 7 9) initial `shouldMatchList` [Coord 7 8, Coord 6 8]
        it "(8,9)"$ destinationsAt (Coord 8 9) initial `shouldMatchList` []
        it "(8,2)"$ destinationsAt (Coord 8 2) initial `shouldMatchList`
            [Coord 9 2, Coord 7 2, Coord 6 2, Coord 5 2, Coord 4 2, Coord 3 2]
        it "(2,2)"$ destinationsAt (Coord 2 2) initial `shouldMatchList` []

