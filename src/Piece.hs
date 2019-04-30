module Piece where

import Coordinate

-- |A generic representation of a Tetromino
data Piece = Piece [Coordinate]

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
