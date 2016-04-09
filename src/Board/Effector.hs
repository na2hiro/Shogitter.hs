module Board.Effector
    ( normalEffector
    , othelloEffector
    , goEffector
    , nipEffector
    , underWaterEffector
    , nuclearEffector
    , dondenEffector
    , gravityEffector
    ) where

import Board
import Coord.Const(fourDirections, eightDirections, forward)
import Piece(Piece(..), promoteReverse)
import Color(Color(..))
import Coord
import qualified Data.Set as S
import Control.Monad(foldM)
import Data.Maybe(isJust)

defaultEffector = Effector {
    runEffector = error "Define!",
    runEffectorPut = effect (error "Define `effectPut` for Effector if `effect` uses `from` parameter")
}

normalEffector = defaultEffector {
    runEffector = effect
} where
    effect _ _ = id

othelloEffector = defaultEffector {
    runEffector = effect
} where
    effect _ to board = sets board$ map (changeColor color)$ nipping8 board to
        where Piece color _ _ = unsafeGet board to
              changeColor color (c, Just (Piece _ promoted kind)) = (c, Just$ Piece color promoted kind)

nipping8 = nipping eightDirections
nipping4 = nipping fourDirections

nipping :: [Coord] -> Board -> Coord -> [(Coord, Cell)]
nipping directions board to = concatMap (sliceNipped color board to) directions
    where Piece color _ _ = unsafeGet board to

sliceNipped :: Color -> Board -> Coord -> Coord -> [(Coord, Cell)]
sliceNipped color board base vector = nipped color$ slice board base vector

nipped :: Color -> [(Coord, Cell)] -> [(Coord, Cell)]
nipped color = nipped' []
    where nipped' acc (t@(_, Just (Piece color' _ _)):ts) | color==color' = acc
                                                          | otherwise = nipped' (t:acc) ts
          nipped' _ _ = []

goEffector = defaultEffector {
    runEffector = effect
} where
    effect _ to board = sets board$ map empty$ surrounding byEnemy board to

empty :: Coord -> (Coord, Cell)
empty c = (c, Nothing)

surrounding :: SurroundType -> Board -> Coord -> [Coord]
surrounding by board coord = concatMap (surrounded by board color. (coord +)) fourDirections
    where Piece color _ _ = unsafeGet board coord -- do not evaluate color for `surrounding bySpace` call

surrounded :: SurroundType -> Board -> Color -> Coord -> [Coord]
surrounded by board color coord = maybe [] S.toList$ surrounded' S.empty coord
    where surrounded' :: S.Set Coord -> Coord -> Maybe (S.Set Coord)
          surrounded' set c | (c `elem` set)  = Just set
                            | otherwise = case by board color c of
                                Surrounded -> Just set
                                NotSurrounded -> Nothing
                                Keep -> foldM surrounded' (S.insert c set)$ map (c +) fourDirections

data Surrounded = Surrounded | NotSurrounded | Keep

type SurroundType = Board -> Color -> Coord -> Surrounded

byEnemy :: SurroundType
byEnemy board color c | not (board `inRange` c) || surrounderColor cell = Surrounded
                      | Nothing <- cell = NotSurrounded
                      | otherwise = Keep
    where cell = get board c
          surrounderColor (Just (Piece color' _ _)) | color==color' = True
          surrounderColor _ = False

bySpace :: Board -> Color -> Coord -> Surrounded
bySpace board _ c | not (board `inRange` c) = NotSurrounded
                  | Nothing <- get board c = Surrounded
                  | otherwise = Keep

nipEffector = defaultEffector {
    runEffector = effect
} where
    effect _ to board = sets board$ map empty$ map fst (nipping4 board to) ++ surrounding byEnemy board to

underWaterEffector = defaultEffector {
    runEffector = effect,
    runEffectorPut = effectPut
} where
    effect from to board = sets board$ map empty$ surrounding bySpace board from ++ surrounded bySpace board undefined to
    effectPut _ = id

nuclearEffector = defaultEffector {
    runEffector = effect
} where
    effect _ to board = sets board diffs
        where dests = destinationsAt board to
              diffs = map flipPiece$ filter (isJust. snd)$ map (\c -> (c, get board c)) dests
              flipPiece (c, Just p) = (c, Just$ promoteReverse p)

dondenEffector = defaultEffector {
    runEffector = effect
} where
    effect _ to board = case safeGet board fwd of
        Just (Piece color' promoted' kind') | color/=color' ->
            sets board [(fwd, Just$ Piece color' promoted kind), (to, Just$ Piece color promoted' kind')]
        _ -> board
        where Piece color promoted kind = unsafeGet board to
              fwd = addCoord color to forward

gravityEffector = defaultEffector {
    runEffector = effect
} where
    effect _ _ board = sets board$ gravity board

gravity :: Board -> [(Coord, Cell)]
gravity board = concatMap gravityRow [1..y]
    where (x, y) = size board
          gravityRow y = zip coords$ candidates++repeat Nothing
            where candidates = filter isJust$ map (get board) coords
                  coords = map (flip Coord y) [1..x]
