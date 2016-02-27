-- |
-- Constants of coordinates
module Coord.Const where

import Coord

forward = Coord 0 (-1)
backward = -forward
right = Coord (-1) 0
left = -right
forwardRight = forward+right
forwardLeft = forward+left
backwardRight = backward+right
backwardLeft = backward+left

fourDirections = [forward, backward, right, left]
fourDirectionsSkew = [forwardRight, forwardLeft, backwardRight, backwardLeft]
eightDirections = fourDirections ++ fourDirectionsSkew
