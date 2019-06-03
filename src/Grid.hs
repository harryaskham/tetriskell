module Grid where

import Coordinate
import Piece

import Data.List
import Data.Maybe
import qualified Data.Vector as V

data Square = Empty | Full Color deriving (Eq)
newtype Row = Row (V.Vector Square)
newtype Grid = Grid (V.Vector Row)

instance Show Square where
  show Empty = show Black ++ "·" ++ "\x1b[0m"
  show (Full c) = show c ++ "■" ++ "\x1b[0m"

instance Show Grid where
  show (Grid g) = intercalate "\n" . map show . drop 4 . reverse $ V.toList g

instance Show Row where
  show (Row r) = concatMap show $ V.toList r

instance Show Piece where
  show piece = show $ withPieceUnsafe originPiece (emptyGrid 3 4)
    where
      originPiece = fst $ normaliseToOrigin piece

-- |Creates an empty row.
emptyRow :: Int -> Row
emptyRow x = Row $ V.replicate x Empty

-- |Creates an empty playing grid of the given dimensions.
emptyGrid :: Int -> Int -> Grid
emptyGrid x y = Grid $ V.replicate (y + 4) (emptyRow x)

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
withCoordinate col (Coordinate (x, y)) (Grid g)
  | y < 0 = Nothing
  | y >= length g = Nothing
  | otherwise = do
    newRow <- setX col x $ g V.! y
    return $ Grid $ g V.// [(y, newRow)]

-- |Sets the x value of the given row with the given color if possible.
setX :: Color -> Int -> Row -> Maybe Row
setX col x (Row r)
  | x < 0 = Nothing
  | x >= V.length r = Nothing
  | r V.! x /= Empty = Nothing
  | otherwise = Just $ Row $ r V.// [(x, Full col)]

-- |Places a piece on the grid.
-- |No overlap or bounds checks.
withPieceUnsafe :: Piece -> Grid -> Grid
withPieceUnsafe (Piece [] _) g = g
withPieceUnsafe (Piece (c:cs) col) g = withPieceUnsafe (Piece cs col) (withCoordinateUnsafe col c g)

-- |Fills a single coordinate on the grid with the given color.
-- |No overlap or bounds checks.
withCoordinateUnsafe :: Color -> Coordinate -> Grid -> Grid
withCoordinateUnsafe col (Coordinate (x, y)) (Grid g)
  | y < 0 = Grid g
  | y >= V.length g = Grid g
  | otherwise = Grid $ g V.// [(y, newRow)]
  where
    newRow = setXUnsafe col x (g V.! y) 

-- |Sets the x value of the given row with the given color
-- |No overlap or bounds checks.
setXUnsafe :: Color -> Int -> Row -> Row
setXUnsafe col x (Row r)
  | x < 0 = Row r
  | x >= V.length r = Row r
  | otherwise = Row $ r V.// [(x, Full col)]

rowEmpty :: Row -> Bool
rowEmpty (Row r) = all (== Empty) r

rowFull :: Row -> Bool
rowFull (Row r) = Empty `notElem` r

rowPopulated :: Row -> Bool
rowPopulated = not . rowEmpty

-- |Is the grid full?
isGridComplete :: Grid -> Bool
isGridComplete (Grid g) = or $ rowPopulated <$> V.drop 20 g

-- |Remove and replace any full rows.
flushGrid :: Grid -> Grid
flushGrid (Grid g) = Grid $ withoutFullRows V.++ newEmptyRows
  where
    (Row firstRow) = g V.! 0
    fullCount = V.length $ V.filter rowFull g
    newEmptyRows = V.replicate fullCount $ emptyRow (V.length firstRow)
    withoutFullRows = V.filter (not . rowFull) g

-- |Count the number of gaps per row.
rowGaps :: Row -> Int
rowGaps (Row r) = length $ V.filter (uncurry (/=)) (V.zip r $ V.drop 1 r)

-- |Count the number of vertical gaps between two rows.
verticalGaps2 :: (Row, Row) -> Int
verticalGaps2 (Row r1, Row r2) = length $ V.filter (uncurry (/=)) (V.zip r1 r2)

-- |Count the number of vertical gaps in the grid
verticalGaps :: Grid -> Int
verticalGaps (Grid g) = sum $ fmap verticalGaps2 (V.zip g $ V.drop 1 g)

-- |How many gaps in the entire game?
horizontalGaps :: Grid -> Int
horizontalGaps (Grid g) = sum $ fmap rowGaps g

-- |Get the index of the first row that has no contents.
lowestEmptyRow :: Grid -> Int
lowestEmptyRow (Grid g) = fromMaybe 0 $ V.findIndex (== True) (fmap rowEmpty g)
