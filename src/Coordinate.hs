module Coordinate where

data Coordinate = Coordinate (Int, Int)

-- |Moves a coordinate by the given amounts.
moveCoordinate :: Int -> Int -> Coordinate -> Coordinate
moveCoordinate x y (Coordinate (x', y')) = Coordinate (x + x', y + y')
