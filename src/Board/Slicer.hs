module Board.Slicer
  ( normalSlicer
  , loopSlicer
  , donutSlicer
  , reflectSlicer
  , slicers
  ) where

import Board
import Coord

data Range
  = Below
  | InRange
  | Above

slicers :: [Slicer]
slicers = [normalSlicer, loopSlicer, donutSlicer, reflectSlicer]

normalSlicer =
  Slicer
    {runRegularity = True, runSliceAsCoord = sliceAsCoord, slicerId = "normal"}
  where
    sliceAsCoord board base vec = slice' (base + vec) vec
      where
        slice' now vec =
          if board `inRange` now
            then now : slice' (now + vec) vec
            else []

loopSlicer =
  Slicer
    {runRegularity = False, runSliceAsCoord = sliceAsCoord, slicerId = "loop"}
  where
    sliceAsCoord board base vec = slice' (base + vec) vec
      where
        bs = bounds board
        slice' now vec =
          case bs `inRangeY` now of
            InRange ->
              case bs `inRangeX` now of
                InRange -> now : slice' (now + vec) vec
                _ ->
                  let now' = modC bs now
                   in now' : slice' (now' + vec) vec
            _ -> []

donutSlicer =
  Slicer
    {runRegularity = False, runSliceAsCoord = sliceAsCoord, slicerId = "donut"}
  where
    sliceAsCoord board base vec = slice' (base + vec) vec
      where
        bs = bounds board
        slice' now vec =
          let now' = modC bs now
           in now' : slice' (now' + vec) vec

reflectSlicer =
  Slicer
    { runRegularity = False
    , runSliceAsCoord = sliceAsCoord
    , slicerId = "reflect"
    }
  where
    sliceAsCoord board base vec = slice' (base + vec) vec
      where
        bs@(min, max) = bounds board
        slice' now vec = now' : slice' (now' + vec') vec'
          where
            now' = Coord x y
            vec' = vec * v1 * v2
            (v1, x) =
              case bs `inRangeX` now of
                InRange -> (Coord 1 1, getX now)
                Below -> (Coord (-1) 1, getX min * 2 - getX now)
                Above -> (Coord (-1) 1, getX max * 2 - getX now)
            (v2, y) =
              case bs `inRangeY` now of
                InRange -> (Coord 1 1, getY now)
                Below -> (Coord 1 (-1), getY min * 2 - getY now)
                Above -> (Coord 1 (-1), getY max * 2 - getY now)

inRangeX :: (Coord, Coord) -> Coord -> Range
inRangeX (Coord minX _, Coord maxX _) (Coord x _)
  | x < minX = Below
  | maxX < x = Above
  | otherwise = InRange

inRangeY :: (Coord, Coord) -> Coord -> Range
inRangeY (Coord _ minY, Coord _ maxY) (Coord _ y)
  | y < minY = Below
  | maxY < y = Above
  | otherwise = InRange

modC :: (Coord, Coord) -> Coord -> Coord
modC (min, max) c =
  min + Coord (getX offsetted `mod` getX max) (getY offsetted `mod` getY max)
  where
    offsetted = c - min
