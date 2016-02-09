module Piece(Coord, Promoted, MoveDef(..), Color(..), Kind(..), Piece(..), move, promote) where

import Color(Color(..))
import Coord

type Promoted = Bool
data MoveDef = Exact Coord
          | Slide Coord deriving (Eq, Show)

data Kind = FU | KY | KE | GI | KI | KA | HI | OU deriving (Show, Ord, Eq, Enum)

kin :: [MoveDef]
kin = map Exact [forwardRight, forward, forwardLeft, right, left, backward]
ex = [forwardRight, forwardLeft, backwardRight, backwardLeft]
plus = [forward, right, left, backward]

move :: Kind -> Promoted -> [MoveDef]
move FU False = [Exact forward]
move KY False = [Slide forward]
move KE False = map Exact [forwardRight+forward, forwardLeft+forward]
move GI False = map Exact [forwardRight, forward, forwardLeft, backwardRight, backwardLeft]
move KI False = kin
move KA False = map Slide ex
move KA True = map Slide ex ++ map Exact plus
move HI False = map Slide plus
move HI True = map Slide plus ++ map Exact ex
move OU False = map Exact (plus++ex)
move _ True = kin

data Piece = Piece Color Promoted Kind deriving (Eq)
promote :: Promoted -> Piece -> Piece
promote promoted (Piece color _ kind) = Piece color promoted kind
instance Show Piece where
    show (Piece color promoted kind) = show color++showKind promoted kind

showKind :: Promoted -> Kind -> String
showKind False kind = show kind
showKind True FU = "TO"
showKind True KY = "NY"
showKind True KE = "NK"
showKind True GI = "NG"
showKind True KA = "UM"
showKind True HI = "RY"

