module PieceSpec where

import Coord (Coord(..))
import Piece
import Test.Hspec

c12 = Coord 1 2

c23 = Coord 2 3

spec :: Spec
spec = do
  describe "move length" $ do it "FU" $ length (moveDefs FU False) `shouldBe` 1
  describe "uniqueMoveDef" $ do
    it "Same Exact" $
      uniqueMoveDef [Exact c12, Exact c23, Exact c12] `shouldMatchList`
      [Exact c12, Exact c23]
    it "Same Slide" $
      uniqueMoveDef [Slide c12, Exact c23, Slide c12] `shouldMatchList`
      [Slide c12, Exact c23]
    it "Slide and Exact" $
      uniqueMoveDef [Slide c12, Exact c23, Exact c12, Slide c23] `shouldMatchList`
      [Slide c12, Slide c23]
    it "Slide and Exact 2" $
      uniqueMoveDef [Exact c12, Slide c23, Slide c12, Exact c23, Exact c12] `shouldMatchList`
      [Slide c12, Slide c23]
