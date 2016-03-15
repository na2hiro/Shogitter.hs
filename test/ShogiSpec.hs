module ShogiSpec where

import Test.Hspec
import Shogi
import Board

initial :: NormalShogi
initial = initialShogi

spec :: Spec
spec = do
    describe "getMoves"$ do
        it "moves on initial board"$ length (getMovesShogi initial) `shouldBe` 30