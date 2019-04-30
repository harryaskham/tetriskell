module Lib
    ( emptyGrid
    , defaultGrid
    , withPiece
    , linePiece
    ) where

import Data.List

data Square = Empty | Full
data Row = Row [Square]
data Grid = Grid [Row]
data Coordinate = Coordinate (Int, Int)
data Piece = Piece [Coordinate]

instance Show Square where
  show Empty = " "
  show Full = "X"

instance Show Grid where
  show (Grid g) = (intercalate "\n") . (map show) . reverse $ g

instance Show Row where
  show (Row r) = concat . (map show) $ r

-- |Creates an empty playing grid of the given dimensions.
emptyGrid :: Int -> Int -> Grid
emptyGrid x y = let row = (Row $ take x $ repeat Empty) in (Grid $ take y $ repeat row)

-- |The default playing field with a 4-line buffer.
defaultGrid :: Grid
defaultGrid = emptyGrid 10 24

-- |A line piece in the bottom-left.
linePiece :: Piece
linePiece = Piece [Coordinate (0, 0), Coordinate (0, 1), Coordinate (0, 2), Coordinate (0, 3)]

-- |Places a piece on the grid if possible.
withPiece :: Piece -> Grid -> Maybe Grid
withPiece (Piece []) g = Just g
withPiece (Piece (c:cs)) g = do
  g <- withCoordinate c g
  withPiece (Piece cs) g

-- |Fills a single coordinate on the grid if possible.
withCoordinate :: Coordinate -> Grid -> Maybe Grid
withCoordinate (Coordinate (x, 0)) (Grid (g:gs)) = do
  (Row r) <- setX x g
  return $ Grid ((Row r):gs)
withCoordinate (Coordinate (x, y)) (Grid (g:gs)) = do
  (Grid gs) <- withCoordinate (Coordinate (0, y-1)) (Grid gs)
  return $ Grid (g:gs)

-- |Sets the x value of the given row if possible.
setX :: Int -> Row -> Maybe Row
setX 0 (Row (Full:_)) = Nothing
setX 0 (Row (Empty:xs)) = Just $ Row (Full:xs)
setX n (Row (x:xs)) = do
  (Row xs) <- setX (n-1) (Row xs)
  return $ Row (x:xs)
