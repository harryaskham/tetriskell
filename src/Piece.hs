module Piece where

import Coordinate

-- |A generic representation of a Tetromino
data Piece = Piece [Coordinate] deriving (Show)

-- |A representation of a bounding box in (bottomLeft, topRight) form.
data BoundingBox = BoundingBox (Coordinate, Coordinate) deriving (Show)

-- |A line piece in the bottom-left.
linePiece :: Piece
linePiece = Piece [Coordinate (0, 0), Coordinate (0, 1), Coordinate (0, 2), Coordinate (0, 3)]

-- |A T piece in the bottom-left.
tPiece :: Piece
tPiece = Piece [Coordinate (0, 0), Coordinate (1, 0), Coordinate (2, 0), Coordinate (1, 1)]

-- |A square piece in the bottom-left.
squarePiece :: Piece
squarePiece = Piece [Coordinate (0, 0), Coordinate (1, 0), Coordinate (0, 1), Coordinate (1, 1)]

-- |An S piece in the bottom-left.
sPiece :: Piece
sPiece = Piece [Coordinate (0, 0), Coordinate (1, 0), Coordinate (1, 1), Coordinate (2, 1)]

-- |A Z piece in the bottom-left.
zPiece :: Piece
zPiece = Piece [Coordinate (0, 1), Coordinate (1, 1), Coordinate (1, 0), Coordinate (2, 0)]

-- |An L piece in the bottom-left.
lPiece :: Piece
lPiece = Piece [Coordinate (0, 2), Coordinate (0, 1), Coordinate (0, 0), Coordinate (1, 0)]

-- |An R piece in the bottom-left.
rPiece :: Piece
rPiece = Piece [Coordinate (0, 2), Coordinate (0, 1), Coordinate (0, 0), Coordinate (1, 2)]

-- |A generator for pieces appearing in the top-middle.
-- TODO: Introduce randomness.
allPiecesAtTop :: [Piece]
allPiecesAtTop = map (movePiece 4 20) $ cycle [rPiece, lPiece, linePiece, sPiece, squarePiece, zPiece]

-- |Moves a piece by the given distances.
-- |No bounds checks - these are the game / grid's responsibility.
movePiece :: Int -> Int -> Piece -> Piece
movePiece x y (Piece cs) = Piece $ map (moveCoordinate x y) cs

-- |Gets bounding box for the given piece represented as (bottom-left, top-right).
boundingBox :: Piece -> BoundingBox
boundingBox (Piece cs) = BoundingBox (Coordinate (leftMost, bottomMost), Coordinate (topMost, rightMost))
  where
    leftMost = minimum $ map (\(Coordinate c) -> fst c) cs
    bottomMost = minimum $ map (\(Coordinate c) -> snd c) cs
    topMost = maximum $ map (\(Coordinate c) -> fst c) cs
    rightMost = maximum $ map (\(Coordinate c) -> snd c) cs

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
rotate :: RotationDirection -> Piece -> Piece
rotate rd p = undoNormalisation rotatedPieceAtOrigin undoC
  where
    rotateCoordinate = case rd of
                         CW -> rotateCoordinateCw
                         CCW -> rotateCoordinateCcw
    ((Piece cs), undoC) = normaliseToOrigin p
    rotatedPieceAtOrigin = (Piece $ map rotateCoordinate cs)
