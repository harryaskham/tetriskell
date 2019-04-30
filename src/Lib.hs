module Lib
    ( emptyGrid
    , defaultGrid
    , withPiece
    , linePiece
    ) where

import Data.List

data Square = O | X deriving (Show)
type Row = [Square]
type Grid = [Row]
type Coordinate = (Int, Int)
type Piece = [Coordinate]

-- |Creates an empty playing grid of the given dimensions.
emptyGrid :: Int -> Int -> Grid
emptyGrid x y = let row = take x $ repeat O in take y $ repeat row

-- |The default playing field with a 4-line buffer.
defaultGrid :: Grid
defaultGrid = emptyGrid 10 24

-- |A line piece in the bottom-left.
linePiece :: Piece
linePiece = [(0, 0), (0, 1), (0, 2), (0, 3)]

-- |Places a piece on the grid if possible.
withPiece :: Piece -> Grid -> Maybe Grid
withPiece [] g = Just g
withPiece (p:ps) g = do
  g <- withCoordinate p g
  withPiece ps g

-- |Fills a single coordinate on the grid if possible.
withCoordinate :: Coordinate -> Grid -> Maybe Grid
withCoordinate (x, 0) (g:gs) = do
  g <- setX x g
  return (g:gs)
withCoordinate (x, y) (g:gs) = do
  gs <- withCoordinate (0, y-1) gs
  return (g:gs)

-- |Sets the x value of the given row if possible.
setX :: Int -> Row -> Maybe Row
setX 0 (X:_) = Nothing
setX 0 (O:xs) = Just (X:xs)
setX n (x:xs) = do
  xs <- setX (n-1) xs
  return (x:xs)
