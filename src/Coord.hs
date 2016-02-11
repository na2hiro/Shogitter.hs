-- | Coordinate of pieces on boards
module Coord(Coord(..), getX, getY) where

import Data.Ix

-- | Coordinate of pieces on boards
data Coord = Coord Int Int deriving (Eq, Ord)

instance Show Coord where
    show (Coord x y) = show (x,y)

-- | Element-wise operations
--
-- >>> Coord 1 2 + Coord 3 4 == Coord 4 6
-- True
-- >>> Coord 1 2 - Coord 3 4 == Coord (-2) (-2)
-- True
instance Num Coord where
    (Coord x y) + (Coord x' y') = Coord (x+x') (y+y')
    (Coord x y) - (Coord x' y') = Coord (x-x') (y-y')
    (Coord x y) * (Coord x' y') = Coord (x*x') (y*y')
    abs (Coord x y)  = Coord (abs x) (abs y)
    signum (Coord x y)  = Coord (signum x) (signum y)
    fromInteger x  = let x' = fromInteger x in Coord x' x'

instance Ix Coord where
    range (c1, c2) = map fromTuple$ range (toTuple c1, toTuple c2)
    inRange (c1, c2) c = inRange (toTuple c1, toTuple c2)$ toTuple c
    index (c1, c2) c = index (toTuple c1, toTuple c2)$ toTuple c

-- |
-- Get x of the Coord
--
-- >>> getX (Coord 3 5)
-- 3
getX :: Coord -> Int
getX (Coord x _) = x

-- |
-- Get y of the Coord
--
-- >>> getY (Coord 3 5)
-- 5
getY :: Coord -> Int
getY (Coord _ y) = y

-- |
-- From Coord to tuple
--
-- >>> toTuple (Coord 3 5) == (3,5)
-- True
toTuple :: Coord -> (Int, Int)
toTuple (Coord x y) = (x, y)

-- |
-- From tuple to Coord
--
-- >>> fromTuple (3,5) == Coord 3 5
-- True
fromTuple :: (Int, Int) -> Coord
fromTuple = uncurry Coord
