module Piece
  ( Coord
  , Promoted
  , MoveDef(..)
  , Color(..)
  , Kind(..)
  , Piece(..)
  , moveDefs
  , promotable
  , promote
  , promoteReverse
  , uniqueMoveDef
  , fromJKFKindColor
  , toJKFKindColor
  ) where

import Color (Color(..))
import Coord
import Coord.Const
import Data.List (sort)

type Promoted = Bool

data MoveDef
  = Exact Coord
  | Slide Coord
  deriving (Eq, Show)

instance Ord MoveDef where
  md1 <= md2 = getCoord md1 <= getCoord md2

getCoord :: MoveDef -> Coord
getCoord (Exact c) = c
getCoord (Slide c) = c

data Kind
  = FU
  | KY
  | KE
  | GI
  | KI
  | KA
  | HI
  | OU
  deriving (Show, Read, Ord, Eq, Enum)

kin :: [MoveDef]
kin = map Exact [forwardRight, forward, forwardLeft, right, left, backward]

moveDefs :: Kind -> Promoted -> [MoveDef]
moveDefs FU False = [Exact forward]
moveDefs KY False = [Slide forward]
moveDefs KE False = map Exact [forwardRight + forward, forwardLeft + forward]
moveDefs GI False =
  map Exact [forwardRight, forward, forwardLeft, backwardRight, backwardLeft]
moveDefs KI False = kin
moveDefs KA False = map Slide fourDirectionsSkew
moveDefs KA True = map Slide fourDirectionsSkew ++ map Exact fourDirections
moveDefs HI False = map Slide fourDirections
moveDefs HI True = map Slide fourDirections ++ map Exact fourDirectionsSkew
moveDefs OU False = map Exact eightDirections
moveDefs _ True = kin

data Piece =
  Piece Color
        Promoted
        Kind
  deriving (Eq)

promote :: Promoted -> Piece -> Piece
promote promoted (Piece color _ kind) =
  Piece color (promotable kind && promoted) kind

promoteReverse :: Piece -> Piece
promoteReverse (Piece color promoted kind) =
  Piece color (promotable kind && not promoted) kind

instance Show Piece where
  show (Piece color promoted kind) = show color ++ showKind promoted kind

showKind :: Promoted -> Kind -> String
showKind False kind = show kind
showKind True FU = "TO"
showKind True KY = "NY"
showKind True KE = "NK"
showKind True GI = "NG"
showKind True KA = "UM"
showKind True HI = "RY"

-- We identify pieces by (Kind, Promote), though JKF identify by kind including promoted kind
toJKFKindColor :: Piece -> (String, Color)
toJKFKindColor (Piece color promoted kind) = (showKind promoted kind, color)

fromJKFKindColor :: String -> Color -> Piece
fromJKFKindColor k c = Piece c promoted kind
  where
    (promoted, kind) = fromJKFKind k

fromJKFKind :: String -> (Promoted, Kind)
fromJKFKind "TO" = (True, FU)
fromJKFKind "NY" = (True, KY)
fromJKFKind "NK" = (True, KE)
fromJKFKind "NG" = (True, GI)
fromJKFKind "UM" = (True, KA)
fromJKFKind "RY" = (True, HI)
fromJKFKind k = (False, read k)

promotable :: Kind -> Bool
promotable OU = False
promotable KI = False
promotable _ = True

uniqueMoveDef :: [MoveDef] -> [MoveDef]
uniqueMoveDef a = uniq $ sort a
  where
    uniq [] = []
    uniq [x] = [x]
    uniq (x:y:xs) =
      if sameC x y
        then case x of
               Exact _ -> uniq (y : xs)
               _ -> uniq (x : xs)
        else x : uniq (y : xs)
    sameC md1 md2 = getCoord md1 == getCoord md2
