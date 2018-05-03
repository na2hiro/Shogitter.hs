module Shogi.Const where

import Shogi(Shogi(..), initialHistory)
import Shogi.Judge(normalJudge)
import Piece(Color(..))
import Board.Const(initialBoard)
import Hands(initialHands)

initialShogi = Shogi {
    getJudge = normalJudge,

    history = initialHistory,
    turn = Black,
    board = initialBoard,
    hands = initialHands
}

{-
yaneuraohCsa =
    "P1-KY-KE *  * -OU *  * -KE-KY\n"++
    "P2 *  *  * -GI-KI-GI-KI *  * \n"++
    "P3-FU * -FU-FU-FU-FU-FU-FU-KA\n"++
    "P4 *  *  *  *  *  *  *  * -FU\n"++
    "P5 *  *  *  *  *  *  *  *  * \n"++
    "P6 *  * +FU+KA-HI * +FU+HI * \n"++
    "P7+FU+FU+KE+FU+FU+FU+KE * +FU\n"++
    "P8 *  * +KI+GI+KI+GI *  *  * \n"++
    "P9+KY *  *  * +OU *  *  * +KY\n"++
    "P+00FU\n"++
    "P-00FU\n"++
    "+"
{-
yaneuraohBoard :: Vector Cell
yaneuraohBoard = fromList. concat. transpose$ gote++replicate 3 four++sente
    where one = map Just $ [KY .. KI]++OU:[KI,GI .. KY]
    [[Just KY, Just KE, Nothing, Nothing, Just OU, Nothing, Nothing, Just KE, Just KY],
    replicate 3 Nothing++map Just [GI,KI,GI,KI]++[Nothing,Nothing],
    [Just FU,Nothing]++map Just [FU,FU,FU,FU,FU,FU,KA],
    replicate 8 Nothing++[Just FU],
    replicate 9 Nothing,
    [[Nothing,Nothing,Just$ Piece Black False FU, Just KE, Nothing, Nothing, Just OU, Nothing, Nothing, Just KE, Just KY],
    ]
    "-FU * -FU-FU-FU-FU-FU-FU-KA\n"++
    " *  *  *  *  *  *  *  * -FU\n"++
    " *  *  *  *  *  *  *  *  * \n"++
    " *  * +FU+KA-HI * +FU+HI * \n"++
    "+FU+FU+KE+FU+FU+FU+KE * +FU\n"++
    " *  * +KI+GI+KI+GI *  *  * \n"++
    "+KY *  *  * +OU *  *  * +KY\n"++
          two = Nothing:Just KA:replicate 5 Nothing++[Just HI,Nothing]
          three = replicate 9 $ Just FU
          four = replicate 9 Nothing
          gote :: [[Cell]]
          gote =  map (map (fmap (Piece White False))) [one,two,three]
          sente :: [[Cell]]
          sente = reverse$ map (reverse . map (fmap (\(Piece _ _ kind)->Piece Black False kind))) gote
-}
yaneuraohProblem = (parseCsa yaneuraohCsa) {
    getJudge = mateJudge
}
-}
