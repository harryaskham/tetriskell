module Coordinate where

-- |A representation of a coordinate on the playing field.
data Coordinate = Coordinate (Int, Int)

-- |Moves a coordinate by the given amounts.
moveCoordinate :: Int -> Int -> Coordinate -> Coordinate
moveCoordinate x y (Coordinate (x', y')) = Coordinate (x+x', y+y')
