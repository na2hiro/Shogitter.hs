module ColorSpec where

import Color
import Test.Hspec

spec :: Spec
spec = do
  describe "show" $ do
    it "Black" $ show Black `shouldBe` "+"
    it "White" $ show White `shouldBe` "-"
  describe "opposite" $ do
    it "Black" $ opposite Black `shouldBe` White
    it "White" $ opposite White `shouldBe` Black
