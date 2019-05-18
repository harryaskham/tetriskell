module Grid where

import Coordinate
import Piece

import Data.List

data Square = Empty | Full Color deriving (Eq)
data Row = Row [Square]
data Grid = Grid [Row]

instance Show Square where
  show Empty = show Black ++ "·" ++ "\x1b[0m"
  show (Full c) = show c ++ "■" ++ "\x1b[0m"

instance Show Grid where
  show (Grid g) = intercalate "\n" . map show . drop 4 . reverse $ g

instance Show Row where
  show (Row r) = concat . (map show) $ r

instance Show Piece where
  show piece = show $ withPieceUnsafe originPiece (emptyGrid 3 4)
    where
      (originPiece, _) = normaliseToOrigin piece

-- |Creates an empty row.
emptyRow :: Int -> Row
emptyRow x = Row $ take x $ repeat Empty

-- |Creates an empty playing grid of the given dimensions.
emptyGrid :: Int -> Int -> Grid
emptyGrid x y = Grid $ take (y + 4) $ repeat (emptyRow x)

-- |The default playing field with a 4-line buffer.
defaultGrid :: Grid
defaultGrid = emptyGrid 10 20

-- |Places a piece on the grid.
-- |Nothing if the piece doesn't fit.
withPiece :: Piece -> Grid -> Maybe Grid
withPiece (Piece [] _) g = Just g
withPiece (Piece (c:cs) col) g = do
  g <- withCoordinate col c g
  withPiece (Piece cs col) g

-- |Fills a single coordinate on the grid with the given color if possible.
withCoordinate :: Color -> Coordinate -> Grid -> Maybe Grid
withCoordinate col (Coordinate (x, 0)) (Grid (g:gs)) = do
  (Row r) <- setX col x g
  return $ Grid ((Row r):gs)
withCoordinate col (Coordinate (x, y)) (Grid ((Row g):gs))
  | x < 0 = Nothing
  | y < 0 = Nothing
  | x >= length g = Nothing
  | y >= length gs = Nothing
  | otherwise = do
    (Grid gs) <- withCoordinate col (Coordinate (x, y-1)) (Grid gs)
    return $ Grid ((Row g):gs)

-- |Sets the x value of the given row with the given color if possible.
setX :: Color -> Int -> Row -> Maybe Row
setX _ 0 (Row ((Full _):_)) = Nothing
setX col 0 (Row (Empty:xs)) = Just $ Row ((Full col):xs)
setX col n (Row row@(x:xs))
  | n < 0 = Nothing
  | n >= length row = Nothing
  | otherwise = do
    (Row xs) <- setX col (n-1) (Row xs)
    return $ Row (x:xs)

-- |Places a piece on the grid.
-- |No overlap or bounds checks.
withPieceUnsafe :: Piece -> Grid -> Grid
withPieceUnsafe (Piece [] _) g = g
withPieceUnsafe (Piece (c:cs) col) g = withPieceUnsafe (Piece cs col) (withCoordinateUnsafe col c g)

-- |Fills a single coordinate on the grid with the given color.
-- |No overlap or bounds checks.
withCoordinateUnsafe :: Color -> Coordinate -> Grid -> Grid
withCoordinateUnsafe _ _ (Grid []) = Grid []
withCoordinateUnsafe col (Coordinate (x, 0)) (Grid (g:gs)) = Grid ((setXUnsafe col x g):gs)
withCoordinateUnsafe col (Coordinate (x, y)) (Grid ((Row g):gs)) = Grid ((Row g):rest)
  where
    (Grid rest) = withCoordinateUnsafe col (Coordinate (x, y-1)) (Grid gs)

-- |Sets the x value of the given row with the given color
-- |No overlap or bounds checks.
setXUnsafe :: Color -> Int -> Row -> Row
setXUnsafe col 0 (Row (_:xs)) = Row ((Full col):xs)
setXUnsafe col n (Row (x:xs)) = Row (x:rest)
  where
    (Row rest) = setXUnsafe col (n-1) (Row xs)

rowEmpty :: Row -> Bool
rowEmpty (Row r) = all (== Empty) r

-- |Remove and replace any full rows.
flushGrid :: Grid -> Grid
flushGrid (Grid []) = Grid []
flushGrid (Grid ((Row r):rs)) =
  if all (/= Empty) r then
    Grid (rest ++ [emptyRow (length r)])
  else
    Grid ((Row r):rest)
  where
    (Grid rest) = flushGrid $ Grid rs
