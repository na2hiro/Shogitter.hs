module Shogi.JudgeSpec where

import Test.Hspec
import Coord
import Piece(Piece(..), Color(..), Kind(..))
import Hands(initialHands, addToHands)
import Shogi
import Shogi.Const(initialShogi)
import Shogi.Judge
import Board(Move(..), sets, Board(..))
import Board.Const(initialBoard)
import Board.Effector(underWaterEffector)

underWaterShogi = initialShogi {
    board = initialBoard { getEffector = underWaterEffector }
}

multipleUnsafeDoMove = foldl (flip unsafeDoMove)

spec :: Spec
spec = do
    describe "normalJudge"$
        let initial = initialShogi { getJudge = normalJudge }
            s = unsafeDoMove (Move (Coord 5 7) (Coord 5 1) True) initial
            initial' = underWaterShogi { getJudge = normalJudge }
            s' = unsafeDoMove (Move (Coord 5 9) (Coord 5 5) False) initial' in do
        it "initial"$ judge initial `shouldBe` Nothing
        it "pick enemy"$ judge s `shouldBe` Nothing
        it "lost my OU"$ judge s' `shouldBe` Nothing

    describe "AbsentJudge"$
        let initial = initialShogi { getJudge = absentJudge }
            s = unsafeDoMove (Move (Coord 5 7) (Coord 5 1) True) initial
            initial' = underWaterShogi { getJudge = absentJudge }
            s' = unsafeDoMove (Move (Coord 5 9) (Coord 5 5) False) initial' in do
        it "initial"$ judge initial `shouldBe` Nothing
        it "pick enemy"$ judge s `shouldBe` Just Win
        it "lost my OU"$ judge s' `shouldBe` Just Lose

    describe "MateJudge"$
        let initial = initialShogi { getJudge = mateJudge }
            s = unsafeDoMove (Move (Coord 5 7) (Coord 5 2) True) initial
            s' = unsafeDoMove (Move (Coord 5 9) (Coord 5 4) False) initial
            s'' = unsafeDoMove (Move (Coord 5 9) (Coord 5 2) False) initial in do
        it "initial"$ judge initial `shouldBe` Nothing
        it "mate enemy"$ judge s `shouldBe` Just Win
        it "mate me"$ judge s' `shouldBe` Just Lose
        it "mate both"$ judge s'' `shouldBe` Just Lose
        it "lost my OU"$ pendingWith "multiple judges"
        it "lost enemy's OU"$ pendingWith "multiple judges"

    describe "CheckMateJudge"$
        let initial = initialShogi { getJudge = checkMateJudge }
            moves = [Move (Coord 2 7) (Coord 2 6) False, Move (Coord 5 1) (Coord 4 2) False,
                     Move (Coord 2 6) (Coord 2 5) False, Move (Coord 4 2) (Coord 3 2) False,
                     Move (Coord 2 5) (Coord 2 4) False, Move (Coord 8 2) (Coord 4 2) False,
                     Move (Coord 2 4) (Coord 2 3) True]
            s = multipleUnsafeDoMove initial moves
            s' = unsafeDoMove (Move (Coord 5 9) (Coord 5 4) False) initial
            s'' = unsafeDoMove (Move (Coord 5 9) (Coord 5 2) False) initial in do
        it "initial"$ judge initial `shouldBe` Nothing
        it "check mate enemy"$ judge s `shouldBe` Just Win
        it "mate me"$ judge s' `shouldBe` Just Lose
        it "mate both"$ judge s'' `shouldBe` Just Lose
        it "lost my OU"$ pendingWith "multiple judges"
        it "lost enemy's OU"$ pendingWith "multiple judges"

    describe "TryJudge"$
        let initial = initialShogi { getJudge = tryJudge }
            moves = [Move (Coord 5 9) (Coord 6 8) False, Move (Coord 5 1) (Coord 5 9) False]
            s = multipleUnsafeDoMove initial moves
            s' = unsafeDoMove (Move (Coord 6 8) (Coord 5 1) False) s in do
        it "initial"$ judge initial `shouldBe` Nothing
        it "try"$ judge s `shouldBe` Just Win
        it "try both"$ judge s' `shouldBe` Just Lose
        it "lost my OU"$ pendingWith "multiple judges"
        it "lost enemy's OU"$ pendingWith "multiple judges"

    describe "OthelloJudge"$
        let initial = initialShogi { getJudge = othelloJudge }
            fu color = (Just$ Piece color False FU)
            diffs = [(Coord x y, fu White) | x<-[1..9], y<-[2,4,5]]++[(Coord x y, fu Black)|x<-[1..9], y<-[6,8]]
            s = initial { board = sets initialBoard diffs } in do
        it "initial"$ judge initial `shouldBe` Nothing
        it "win"$ judge s `shouldBe` Just Win

    describe "GomokuJudge"$
        let initial = initialShogi { getJudge = gomokuJudge }
            fu color = (Just$ Piece color False FU)
            diffs = [(Coord 5 y, Nothing) | y<-[1,3,7,9]]
            s = initial { board = sets initialBoard diffs }
            s' = unsafeDoMove (Move (Coord 4 9) (Coord 5 9) False) s
            s'' = unsafeDoMove (Move (Coord 4 9) (Coord 5 7) False) s
            s''' = unsafeDoMove (Move (Coord 4 9) (Coord 5 3) False) s in do
        it "initial"$ judge initial `shouldBe` Just Lose
        it "no"$ judge s `shouldBe` Nothing
        it "5"$ judge s' `shouldBe` Just Win
        it "5+"$ judge s'' `shouldBe` Just Win
        it "mixed 5"$ judge s''' `shouldBe` Nothing

    describe "WinHandCountJudge"$
        let fu color = (Just$ Piece color False FU)
            hands = foldr (\kind hands->addToHands White kind$ addToHands Black kind hands) initialHands$ replicate 2 FU
            s = initialShogi { getJudge = winHandCountJudge, hands = hands }
            s' = unsafeDoMove (Move (Coord 8 8) (Coord 3 3) False) s in do
        it "2, 2"$ judge s `shouldBe` Nothing
        it "3"$ judge s' `shouldBe` Just Win

    describe "LoseHandCountJudge"$
        it ""$ pendingWith "multiple judges"
