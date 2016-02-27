module Piece(Coord, Promoted, MoveDef(..), Color(..), Kind(..), Piece(..), moveDefs, promote, uniqueMoveDef) where

import Color(Color(..))
import Coord
import Coord.Const
import Data.List(sort)

type Promoted = Bool
data MoveDef = Exact Coord
          | Slide Coord deriving (Eq, Show)

instance Ord MoveDef where
    md1 <= md2 = getCoord md1 <= getCoord md2

getCoord :: MoveDef -> Coord
getCoord (Exact c) = c
getCoord (Slide c) = c

data Kind = FU | KY | KE | GI | KI | KA | HI | OU deriving (Show, Ord, Eq, Enum)

kin :: [MoveDef]
kin = map Exact [forwardRight, forward, forwardLeft, right, left, backward]
ex = [forwardRight, forwardLeft, backwardRight, backwardLeft]
plus = [forward, right, left, backward]

moveDefs :: Kind -> Promoted -> [MoveDef]
moveDefs FU False = [Exact forward]
moveDefs KY False = [Slide forward]
moveDefs KE False = map Exact [forwardRight+forward, forwardLeft+forward]
moveDefs GI False = map Exact [forwardRight, forward, forwardLeft, backwardRight, backwardLeft]
moveDefs KI False = kin
moveDefs KA False = map Slide ex
moveDefs KA True = map Slide ex ++ map Exact plus
moveDefs HI False = map Slide plus
moveDefs HI True = map Slide plus ++ map Exact ex
moveDefs OU False = map Exact (plus++ex)
moveDefs _ True = kin

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

uniqueMoveDef :: [MoveDef] -> [MoveDef]
uniqueMoveDef a = uniq$ sort a
    where uniq [] = []
          uniq [x] = [x]
          uniq (x:y:xs) = if sameC x y
              then case x of
                Exact _ ->uniq (y:xs)
                _ -> uniq (x:xs)
              else x:uniq(y:xs)
          sameC md1 md2 = getCoord md1 == getCoord md2
          isExact (Exact _) = True
          isExact _ = False
