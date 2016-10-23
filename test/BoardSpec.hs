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
    describe "getMovesEach == getMoves"$
        let initialEach = getMovesEach initial ([], [])
            initialWithBEach = getMovesEach initial ([FU], [])
            initialBothEach = getMovesEach initial ([KA], [HI]) in do
        it "initial Black"$ getMoves Black initial [] `shouldMatchList` fst initialEach
        it "initial White"$ getMoves White initial [] `shouldMatchList` snd initialEach
        it "initial Black with Black FU"$ getMoves Black initial [FU] `shouldMatchList` fst initialWithBEach
        it "initial White with Black FU"$ getMoves White initial [] `shouldMatchList` snd initialWithBEach
        it "initial Black Both"$ getMoves Black initial [KA] `shouldMatchList` fst initialBothEach
        it "initial White Both"$ getMoves White initial [HI] `shouldMatchList` snd initialBothEach
    -- TODO: Use QuickCheck
    describe "coordToInt. intToCoord == id"$
        let b = initialBoard{size=(2,3)} in do
        it "initial Black"$ coordToInt initialBoard (intToCoord initialBoard 0) `shouldBe` 0
        it "initial Black"$ coordToInt initialBoard (intToCoord initialBoard 1) `shouldBe` 1
        it "initial Black"$ coordToInt initialBoard (intToCoord initialBoard 2) `shouldBe` 2
        it "initial Black"$ coordToInt initialBoard (intToCoord initialBoard 80) `shouldBe` 80
        it "initial Black"$ coordToInt b (intToCoord b 0) `shouldBe` 0
        it "initial Black"$ coordToInt b (intToCoord b 1) `shouldBe` 1
        it "initial Black"$ coordToInt b (intToCoord b 2) `shouldBe` 2
        it "initial Black"$ coordToInt b (intToCoord b 3) `shouldBe` 3
        it "initial Black"$ coordToInt b (intToCoord b 4) `shouldBe` 4
        it "initial Black"$ coordToInt b (intToCoord b 5) `shouldBe` 5
    describe "intToCoord. coordToInt == id"$
        let b = initialBoard{size=(2,3)}
            c11 = Coord 1 1
            c12 = Coord 1 2
            c13 = Coord 1 3 in do
        it "initial Black"$ intToCoord initialBoard (coordToInt initialBoard c11) `shouldBe` c11
        it "initial Black"$ intToCoord initialBoard (coordToInt initialBoard c12) `shouldBe` c12
        it "initial Black"$ intToCoord initialBoard (coordToInt initialBoard c13) `shouldBe` c13
        it "initial Black"$ intToCoord initialBoard (coordToInt initialBoard (Coord 9 9)) `shouldBe` Coord 9 9
        it "initial Black"$ intToCoord b (coordToInt b c11) `shouldBe` c11
        it "initial Black"$ intToCoord b (coordToInt b c12) `shouldBe` c12
        it "initial Black"$ intToCoord b (coordToInt b c13) `shouldBe` c13
        it "initial Black"$ intToCoord b (coordToInt b (Coord 2 1)) `shouldBe` Coord 2 1
        it "initial Black"$ intToCoord b (coordToInt b (Coord 2 2)) `shouldBe` Coord 2 2
        it "initial Black"$ intToCoord b (coordToInt b (Coord 2 3)) `shouldBe` Coord 2 3
    describe "intToCoord"$
        let b = initialBoard{size=(2,3)}
            c11 = Coord 1 1
            c12 = Coord 1 2
            c13 = Coord 1 3 in do
        it "initial Black"$ intToCoord initialBoard 0 `shouldBe` c11
        it "initial Black"$ intToCoord initialBoard 1 `shouldBe` c12
        it "initial Black"$ intToCoord initialBoard 2 `shouldBe` c13
        it "initial Black"$ intToCoord initialBoard 80 `shouldBe` Coord 9 9
        it "initial Black"$ intToCoord b 0 `shouldBe` c11
        it "initial Black"$ intToCoord b 1 `shouldBe` c12
        it "initial Black"$ intToCoord b 2 `shouldBe` c13
        it "initial Black"$ intToCoord b 3 `shouldBe` Coord 2 1
        it "initial Black"$ intToCoord b 4 `shouldBe` Coord 2 2
        it "initial Black"$ intToCoord b 5 `shouldBe` Coord 2 3
    describe "coordToInt"$
        let b = initialBoard{size=(2,3)}
            c11 = Coord 1 1
            c12 = Coord 1 2
            c13 = Coord 1 3 in do
        it "initial Black"$ coordToInt initialBoard c11 `shouldBe` 0
        it "initial Black"$ coordToInt initialBoard c12 `shouldBe` 1
        it "initial Black"$ coordToInt initialBoard c13 `shouldBe` 2
        it "initial Black"$ coordToInt initialBoard (Coord 9 9) `shouldBe` 80
        it "initial Black"$ coordToInt b c11 `shouldBe` 0
        it "initial Black"$ coordToInt b c12 `shouldBe` 1
        it "initial Black"$ coordToInt b c13 `shouldBe` 2
        it "initial Black"$ coordToInt b (Coord 2 1) `shouldBe` 3
        it "initial Black"$ coordToInt b (Coord 2 2) `shouldBe` 4
        it "initial Black"$ coordToInt b (Coord 2 3) `shouldBe` 5
