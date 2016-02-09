module Coord where

import Data.Ix

data Coord = Coord !Int !Int deriving (Eq, Ord)

instance Show Coord where
    show (Coord x y) = show (x,y)

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

getX :: Coord -> Int
getX (Coord x _) = x

getY :: Coord -> Int
getY (Coord _ y) = y

toTuple :: Coord -> (Int, Int)
toTuple (Coord x y) = (x, y)

fromTuple :: (Int, Int) -> Coord
fromTuple = uncurry Coord

forward = Coord 0 (-1)
backward = -forward
right = Coord (-1) 0
left = -right
forwardRight = forward+right
forwardLeft = forward+left
backwardRight = backward+right
backwardLeft = backward+left
