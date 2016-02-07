module Piece(Coord, Promoted, MoveDef(..), Color(..), Kind(..), Piece(..), move, promote, opposite, addCoord) where

type Coord = (Int, Int)
type Promoted = Bool
data MoveDef = Exact Coord
          | Slide Coord deriving (Eq, Show)
data Color = Black | White deriving (Eq)
instance Show Color where
    show Black = "+"
    show White = "-"

addCoord :: Color -> Coord -> Coord -> Coord
addCoord Black (x,y) (a,b) = (a+x,b+y)
addCoord White (x,y) (a,b) = (a-x,b-y)

opposite :: Color -> Color
opposite Black = White
opposite White = Black

data Kind = FU | KY | KE | GI | KI | KA | HI | OU deriving (Show, Ord, Eq, Enum)

kin :: [MoveDef]
kin = [Exact (-1, -1), Exact (0, -1), Exact (1, -1), Exact (-1, 0), Exact (1, 0), Exact (0, 1)]

move :: Kind -> Promoted -> [MoveDef]
move FU False = [Exact (0, -1)]
move KY False = [Slide (0, -1)]
move KE False = [Exact (1, -2), Exact (-1, -2)]
move GI False = [Exact (-1, -1), Exact (0, -1), Exact (1, -1), Exact (-1, 1), Exact (1, 1)]
move KI False = kin
move KA False = [Slide (-1, -1), Slide (1, -1), Slide (-1, 1), Slide (1, 1)]
move KA True = [Slide (-1, -1), Slide (1, -1), Slide (-1, 1), Slide (1, 1), Exact (-1, 0), Exact (0, -1), Exact (0, 1), Exact (1, 0)]
move HI False = [Slide (-1, 0), Slide (0, -1), Slide (0, 1), Slide (1, 0)]
move HI True = [Slide (-1, 0), Slide (0, -1), Slide (0, 1), Slide (1, 0), Exact (-1, -1), Exact (1, -1), Exact (-1, 1), Exact (1, 1)]
move OU False = [Exact (-1, -1), Exact (1, -1), Exact (-1, 1), Exact (1, 1), Exact (-1, 0), Exact (0, -1), Exact (0, 1), Exact (1, 0)]
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

