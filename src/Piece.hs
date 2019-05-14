module Piece where

import System.Random

import Coordinate

-- |Representation of piece color.
data Color = Black
           | Red 
           | DBlue
           | LBlue
           | Yellow
           | Green
           | Purple
           | Orange
           deriving (Eq)

instance Show Color where
  show Black = "\x1b[30m"
  show Red = "\x1b[31m"
  show DBlue = "\x1b[34m"
  show LBlue = "\x1b[36m"
  show Yellow = "\x1b[33m"
  show Green = "\x1b[32m"
  show Purple = "\x1b[35m"
  show Orange = "\x1b[37m"

-- |A generic representation of a Tetromino
-- |TODO: Probably more nicely represented as a Record to avoid all the pattern matching for colors.
data Piece = Piece [Coordinate] Color

-- |A representation of a bounding box in (bottomLeft, topRight) form.
data BoundingBox = BoundingBox (Coordinate, Coordinate) deriving (Show)

-- |A line piece in the bottom-left.
linePiece :: Piece
linePiece = Piece [Coordinate (0, 0), Coordinate (0, 1), Coordinate (0, 2), Coordinate (0, 3)] LBlue

-- |A T piece in the bottom-left.
tPiece :: Piece
tPiece = Piece [Coordinate (0, 0), Coordinate (1, 0), Coordinate (2, 0), Coordinate (1, 1)] Purple

-- |A square piece in the bottom-left.
squarePiece :: Piece
squarePiece = Piece [Coordinate (0, 0), Coordinate (1, 0), Coordinate (0, 1), Coordinate (1, 1)] Yellow

-- |An S piece in the bottom-left.
sPiece :: Piece
sPiece = Piece [Coordinate (0, 0), Coordinate (1, 0), Coordinate (1, 1), Coordinate (2, 1)] Green

-- |A Z piece in the bottom-left.
zPiece :: Piece
zPiece = Piece [Coordinate (0, 1), Coordinate (1, 1), Coordinate (1, 0), Coordinate (2, 0)] Red

-- |An L piece in the bottom-left.
lPiece :: Piece
lPiece = Piece [Coordinate (0, 2), Coordinate (0, 1), Coordinate (0, 0), Coordinate (1, 0)] Orange

-- |An R piece in the bottom-left.
rPiece :: Piece
rPiece = Piece [Coordinate (0, 2), Coordinate (0, 1), Coordinate (0, 0), Coordinate (1, 2)] DBlue

-- |Makes the piece black.
makeBlack :: Piece -> Piece
makeBlack (Piece cs _) = Piece cs Black

-- |A generator for pieces appearing in the top-middle.
allPiecesAtTop :: [Piece]
allPiecesAtTop = map (movePiece 4 20) $ cycle [rPiece, lPiece, linePiece, sPiece, squarePiece, zPiece, tPiece]

-- |Gets a random piece at the top of the board.
randomPieceAtTop :: RandomGen g => g -> (Piece, g)
randomPieceAtTop g = (head $ drop n allPiecesAtTop, g')
  where
    (n, g') = randomR (0, 6) g

-- |Moves a piece by the given distances.
-- |No bounds checks - these are the game / grid's responsibility.
movePiece :: Int -> Int -> Piece -> Piece
movePiece x y (Piece cs col) = Piece (map (moveCoordinate x y) cs) col

-- |Gets bounding box for the given piece represented as (bottom-left, top-right).
boundingBox :: Piece -> BoundingBox
boundingBox (Piece cs _) = BoundingBox (Coordinate (leftMost, bottomMost), Coordinate (rightMost, topMost))
  where
    leftMost = minimum $ map (\(Coordinate c) -> fst c) cs
    bottomMost = minimum $ map (\(Coordinate c) -> snd c) cs
    rightMost = maximum $ map (\(Coordinate c) -> snd c) cs
    topMost = maximum $ map (\(Coordinate c) -> fst c) cs

-- |Move the piece to the origin and return the coordinate its bottom-left
-- |bounding box should take.
normaliseToOrigin :: Piece -> (Piece, Coordinate)
normaliseToOrigin p = (movePiece (-leftMost) (-bottomMost) p, Coordinate (leftMost, bottomMost))
  where
    BoundingBox (Coordinate (leftMost, bottomMost), _) = boundingBox p

-- |Undoes the given normalisation.
-- |First pushes the piece to the origin then undoes it.
-- |Means that the bounding box ends up in the same bottom-left.
undoNormalisation :: Piece -> Coordinate -> Piece
undoNormalisation p (Coordinate (x, y)) = movePiece (x - leftMost) (y - bottomMost) p
  where
    (BoundingBox (Coordinate (leftMost, bottomMost), _)) = boundingBox p

-- |The direction of rotation.
data RotationDirection = CW | CCW

-- |Rotates a piece CW by rotating its bounding box.
-- |Does no validation; will need to be fixed in the context of the game.
-- |Only works about the origin, so need to translate / untranslate too.
-- |TODO: Line rotation is about the bottom-left.
rotate :: RotationDirection -> Piece -> Piece
rotate rd p = undoNormalisation rotatedPieceAtOrigin undoC
  where
    rotateCoordinate = case rd of
                         CW -> rotateCoordinateCw
                         CCW -> rotateCoordinateCcw
    ((Piece cs col), undoC) = normaliseToOrigin p
    rotatedPieceAtOrigin = (Piece (map rotateCoordinate cs) col)
