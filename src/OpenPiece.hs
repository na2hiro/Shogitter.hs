module OpenPiece where

import Piece hiding (Piece, Kind(..))

-- open datatype

kin :: [MoveDef]
kin = [Exact (-1, -1), Exact (0, -1), Exact (1, -1), Exact (-1, 0), Exact (1, 0), Exact (0, 1)]

class Piece p where
    move :: p -> Bool -> [MoveDef]

data Fu = Fu
instance Piece Fu where
    move Fu False = [Exact (0, -1)]
    move Fu True = kin

data OpenKy = OpenKy
instance Piece OpenKy where
    move OpenKy False = [Slide (0, -1)]
    move OpenKy True = kin

data Ke = Ke
instance Piece Ke where
    move Ke False = [Exact (1, -2), Exact (-1, -2)]
    move Ke True = kin

data Gi = Gi
instance Piece Gi where
    move Gi False = [Exact (-1, -1), Exact (0, -1), Exact (1, -1), Exact (-1, 1), Exact (1, 1)]
    move Gi True = kin
