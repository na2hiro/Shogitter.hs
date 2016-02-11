module CoordSpec where

import Test.Hspec
import Coord

spec :: Spec
spec = do
    describe "show"$ do
        it "Coord 3 5"$ show (Coord 3 5) `shouldBe` "(3,5)"
    describe "instance Num"$ do
        it "(1,2)+(-3,4)"$ Coord 1 2 + Coord (-3) 4 `shouldBe` Coord (-2) 6
        it "(1,2)-(-3,4)"$ Coord 1 2 - Coord (-3) 4 `shouldBe` Coord 4 (-2)
        it "abs (-3,4)"$ abs (Coord (-3) 4) `shouldBe` Coord 3 4
        it "signum (-3,4)"$ signum (Coord (-3) 4) `shouldBe` Coord (-1) 1
        it "fromInteger (-3)"$ fromInteger(-3) `shouldBe` Coord (-3) (-3)
    describe "getX"$ do
        it "(3,5)"$ getX (Coord 3 5) `shouldBe` 3
    describe "getY"$ do
        it "(3,5)"$ getY (Coord 3 5) `shouldBe` 5
