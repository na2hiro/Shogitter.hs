module ShogiSpec where

import Test.Hspec
import Shogi
import Coord(Coord(..))
import Color(Color(..))
import Piece(Kind(..))
import Hands(toList)
import Board

initial :: NormalShogi
initial = initialShogi

spec :: Spec
spec = do
    describe "getMoves"$ do
        it "moves on initial board"$ length (getMovesShogi initial) `shouldBe` 30

    describe "hands"$
        let s@(Shogi _ _ hands) = unsafeDoMove (Move (Coord 8 8) (Coord 2 2) True) initial
            s'@(Shogi _ _ hands') = unsafeDoMove (Move (Coord 3 1) (Coord 2 2) True) s
            s''@(Shogi _ _ hands'') = unsafeDoMove (Put (Coord 4 5) KA) s' in do
        it "s Black"$ toList Black hands `shouldBe` [(KA, 1)]
        it "s White"$ toList White hands `shouldBe` []
        it "s' Black"$ toList Black hands' `shouldBe` [(KA, 1)]
        it "s' White"$ toList White hands' `shouldBe` [(KA, 1)]
        it "s'' Black"$ toList Black hands'' `shouldBe` []
        it "s'' White"$ toList White hands'' `shouldBe` [(KA, 1)]
