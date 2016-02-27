module Board.Effector
    ( NormalEffector
    , OthelloEffector
    , GoEffector
    , NipEffector
    ) where

import {-# SOURCE #-}Board
import Coord.Const(fourDirections, eightDirections)
import Piece(Piece(..))
import Color(Color(..))
import Coord
import Coord.Const
import Data.List(concatMap)
import qualified Data.Set as S
import Control.Monad(foldM)
import Data.Maybe(maybe)

data NormalEffector
instance Effector NormalEffector where
    effect _ = id

data OthelloEffector
instance Effector OthelloEffector where
    effect to board = sets board$ map (changeColor color)$ nipping8 board to
        where Piece color _ _ = unsafeGet board to
              changeColor color (c, Just (Piece _ promoted kind)) = (c, Just$ Piece color promoted kind)

nipping8 = nipping eightDirections
nipping4 = nipping fourDirections

nipping :: [Coord] -> Board m e a s -> Coord -> [(Coord, Cell)]
nipping directions board to = concatMap (sliceNipped color board to) directions
    where Piece color _ _ = unsafeGet board to

sliceNipped :: Color -> Board m e a s -> Coord -> Coord -> [(Coord, Cell)]
sliceNipped color board@(Board _ _) base vector = nipped color$ slice board base vector

nipped :: Color -> [(Coord, Cell)] -> [(Coord, Cell)]
nipped color = nipped' []
    where nipped' acc (t@(_, Just (Piece color' _ _)):ts) | color==color' = acc
                                                          | otherwise = nipped' (t:acc) ts
          nipped' _ _ = []

data GoEffector
instance Effector GoEffector where
    effect to board = sets board$ map empty$ surrounding board to

empty :: Coord -> (Coord, Cell)
empty c = (c, Nothing)

surrounding :: Board m e a s -> Coord -> [Coord]
surrounding board coord = concatMap (surrounded board color. (coord +)) fourDirections
    where Piece color _ _ = unsafeGet board coord

surrounded :: Board m e a s -> Color -> Coord -> [Coord]
surrounded board@(Board _ _) color coord = maybe [] S.toList$ surrounded' S.empty coord
    where surrounderColor (Just (Piece color' _ _)) | color==color' = True
          surrounderColor _ = False
          surrounded' :: S.Set Coord -> Coord -> Maybe (S.Set Coord)
          surrounded' set c | (c `elem` set) || not (board `inRange` c) = Just set
                            | Nothing <- cell = Nothing
                            | surrounderColor cell = Just set
                            | otherwise = foldM surrounded' (S.insert c set)$ map (c +) fourDirections
            where cell = get board c

data NipEffector
instance Effector NipEffector where
    effect to board = sets board$ map empty$ map fst (nipping4 board to) ++ surrounding board to

