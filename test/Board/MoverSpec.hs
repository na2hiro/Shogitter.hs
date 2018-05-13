module Board.MoverSpec where

import Board (Board(..), Move(..), destinationsAt, get, move, set)
import Board.Const (initialBoard)
import Board.Slicer
import Color (Color(..))
import Coord (Coord(..))
import Piece (Kind(..), Piece(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "NormalMover" $ do
    it "(1,1)->(1,2) TO" $
      let from = Coord 1 1
          board = set initialBoard (from, Just $ Piece Black True FU)
          (moved, _) = move Black (Move from (Coord 1 2) False) board
       in get moved (Coord 1 2) `shouldBe` Just (Piece Black True FU)
    it "(1,1)->(1,2) KI promote:False" $
      let from = Coord 1 1
          board = set initialBoard (from, Just $ Piece Black False KI)
          (moved, _) = move Black (Move from (Coord 1 2) False) board
       in get moved (Coord 1 2) `shouldBe` Just (Piece Black False KI)
    it "(1,1)->(1,2) KI promote:True (Should fail?)" $
      let from = Coord 1 1
          board = set initialBoard (from, Just $ Piece Black False KI)
          (moved, _) = move Black (Move from (Coord 1 2) True) board
       in get moved (Coord 1 2) `shouldBe` Just (Piece Black False KI)
