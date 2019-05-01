module Grid where

import Coordinate
import Piece

import Data.List

data Square = Empty | Full deriving (Eq)
data Row = Row [Square]
data Grid = Grid [Row]

instance Show Square where
  show Empty = "·"
  show Full = "■"

instance Show Grid where
  show (Grid g) = intercalate "\n" . map show . drop 4 . reverse $ g

instance Show Row where
  show (Row r) = concat . (map show) $ r

-- |Creates an empty row.
emptyRow :: Int -> Row
emptyRow x = Row $ take x $ repeat Empty

-- |Creates an empty playing grid of the given dimensions.
emptyGrid :: Int -> Int -> Grid
emptyGrid x y = Grid $ (take (y + 4) $ repeat (emptyRow x))

-- |The default playing field with a 4-line buffer.
defaultGrid :: Grid
defaultGrid = emptyGrid 10 20

-- |Places a piece on the grid.
-- |Nothing if the piece doesn't fit.
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
withCoordinate (Coordinate (x, y)) (Grid ((Row g):gs))
  | x < 0 = Nothing
  | y < 0 = Nothing
  | x >= length g = Nothing
  | y >= length gs = Nothing
  | otherwise = do
    (Grid gs) <- withCoordinate (Coordinate (x, y-1)) (Grid gs)
    return $ Grid ((Row g):gs)

-- |Sets the x value of the given row if possible.
setX :: Int -> Row -> Maybe Row
setX 0 (Row (Full:_)) = Nothing
setX 0 (Row (Empty:xs)) = Just $ Row (Full:xs)
setX n (Row row@(x:xs))
  | n < 0 = Nothing
  | n >= length row = Nothing
  | otherwise = do
    (Row xs) <- setX (n-1) (Row xs)
    return $ Row (x:xs)

-- |Remove and replace any full rows.
flushGrid :: Grid -> Grid
flushGrid (Grid []) = Grid []
flushGrid (Grid ((Row r):rs)) =
  if all (== Full) r then
    Grid (rest ++ [emptyRow (length r)])
  else
    Grid ((Row r):rest)
  where
    (Grid rest) = flushGrid $ Grid rs
