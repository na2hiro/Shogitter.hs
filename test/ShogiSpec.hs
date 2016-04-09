module ShogiSpec where

import Test.Hspec
import Shogi
import Shogi.Const(initialShogi)
import Coord(Coord(..))
import Color(Color(..))
import Piece(Kind(..))
import Hands(toList)
import Board

spec :: Spec
spec = do
    describe "getMoves"$ do
        it "moves on initial board"$ length (getMovesShogi initialShogi) `shouldBe` 30

    describe "hands"$
        let s = unsafeDoMove (Move (Coord 8 8) (Coord 2 2) True) initialShogi
            s' = unsafeDoMove (Move (Coord 3 1) (Coord 2 2) True) s
            s'' = unsafeDoMove (Put (Coord 4 5) KA) s' in do
        it "s Black"$ toList Black (hands s) `shouldBe` [(KA, 1)]
        it "s White"$ toList White (hands s) `shouldBe` []
        it "s' Black"$ toList Black (hands s') `shouldBe` [(KA, 1)]
        it "s' White"$ toList White (hands s') `shouldBe` [(KA, 1)]
        it "s'' Black"$ toList Black (hands s'') `shouldBe` []
        it "s'' White"$ toList White (hands s'') `shouldBe` [(KA, 1)]
