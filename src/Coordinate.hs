module Coordinate where

-- |A representation of a coordinate on the playing field.
newtype Coordinate = Coordinate (Int, Int) deriving (Show)

-- |Moves a coordinate by the given amounts.
moveCoordinate :: Int -> Int -> Coordinate -> Coordinate
moveCoordinate x y (Coordinate (x', y')) = Coordinate (x+x', y+y')

-- |Rotates a single coordinate clockwise about the origin.
rotateCoordinateCw :: Coordinate -> Coordinate
rotateCoordinateCw (Coordinate (x, y)) = Coordinate (-y, x)

-- |Rotates a single coordinate counter-clockwise about the origin.
rotateCoordinateCcw :: Coordinate -> Coordinate
rotateCoordinateCcw (Coordinate (x, y)) = Coordinate (y, -x)
