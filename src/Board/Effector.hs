module Board.Effector
    ( NormalEffector
    , OthelloEffector
    , GoEffector
    , NipEffector
    , UnderWaterEffector
    , NuclearEffector
    , DondenEffector
    , GravityEffector
    ) where

import {-# SOURCE #-}Board
import Coord.Const(fourDirections, eightDirections)
import Piece(Piece(..), promoteReverse)
import Color(Color(..))
import Coord
import Coord.Const
import Data.List(concatMap)
import qualified Data.Set as S
import Control.Monad(foldM)
import Data.Maybe(maybe, isNothing, isJust)

data NormalEffector
instance Effector NormalEffector where
    effect _ _ = id

data OthelloEffector
instance Effector OthelloEffector where
    effect _ to board = sets board$ map (changeColor color)$ nipping8 board to
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
    effect _ to board = sets board$ map empty$ surrounding byEnemy board to

empty :: Coord -> (Coord, Cell)
empty c = (c, Nothing)

surrounding :: SurroundType m e a s -> Board m e a s -> Coord -> [Coord]
surrounding by board coord = concatMap (surrounded by board color. (coord +)) fourDirections
    where Piece color _ _ = unsafeGet board coord -- do not evaluate color for `surrounding bySpace` call

surrounded :: SurroundType m e a s -> Board m e a s -> Color -> Coord -> [Coord]
surrounded by board@(Board _ _) color coord = maybe [] S.toList$ surrounded' S.empty coord
    where surrounded' :: S.Set Coord -> Coord -> Maybe (S.Set Coord)
          surrounded' set c | (c `elem` set)  = Just set
                            | otherwise = case by board color c of
                                Surrounded -> Just set
                                NotSurrounded -> Nothing
                                Keep -> foldM surrounded' (S.insert c set)$ map (c +) fourDirections

data Surrounded = Surrounded | NotSurrounded | Keep

type SurroundType m e a s = Board m e a s -> Color -> Coord -> Surrounded

byEnemy :: SurroundType m e a s
byEnemy board color c | not (board `inRange` c) || surrounderColor cell = Surrounded
                      | Nothing <- cell = NotSurrounded
                      | otherwise = Keep
    where cell = get board c
          surrounderColor (Just (Piece color' _ _)) | color==color' = True
          surrounderColor _ = False

bySpace :: Board m e a s -> Color -> Coord -> Surrounded
bySpace board _ c | not (board `inRange` c) = NotSurrounded
                  | Nothing <- get board c = Surrounded
                  | otherwise = Keep

data NipEffector
instance Effector NipEffector where
    effect _ to board = sets board$ map empty$ map fst (nipping4 board to) ++ surrounding byEnemy board to

data UnderWaterEffector
instance Effector UnderWaterEffector where
    effect from to board = sets board$ map empty$ surrounding bySpace board from ++ surrounded bySpace board undefined to
    effectPut _ = id

data NuclearEffector
instance Effector NuclearEffector where
    effect _ to board = sets board diffs
        where dests = destinationsAt board to
              diffs = map flipPiece$ filter (isJust. snd)$ map (\c -> (c, get board c)) dests
              flipPiece (c, Just p) = (c, Just$ promoteReverse p)

data DondenEffector
instance Effector DondenEffector where
    effect _ to board = case safeGet board fwd of
        Just (Piece color' promoted' kind') | color/=color' ->
            sets board [(fwd, Just$ Piece color' promoted kind), (to, Just$ Piece color promoted' kind')]
        _ -> board
        where Piece color promoted kind = unsafeGet board to
              fwd = addCoord color to forward

data GravityEffector
instance Effector GravityEffector where
    effect _ _ board = sets board$ gravity board

gravity :: Board m e a s -> [(Coord, Cell)]
gravity board@(Board (x,y) v) = concatMap gravityRow [1..y]
    where gravityRow y = zip coords$ candidates++repeat Nothing
            where candidates = filter isJust$ map (get board) coords
                  coords = map (flip Coord y) [1..x]
