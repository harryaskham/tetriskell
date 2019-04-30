{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Data.List
import Control.Lens hiding (Empty)

data Square = Empty | Full
data Row = Row [Square]
data Grid = Grid [Row]
data Coordinate = Coordinate (Int, Int)
data Piece = Piece [Coordinate]
data Game = Game { _grid :: Grid, _piece :: Piece, _pieceGen :: [Piece]}

makeLenses ''Game

-- Strategy:
-- Keep a fixed reference to the placed-grid - items that are fixed.
-- Keep a piece separate to this
-- The display grid is grid with piece, but grid remains fixed
-- Piece rotation... how?
-- Have a notion of a 'turn' where we attempt multiple things - input, step-down, etc and if stepdown fails fix it.

instance Show Game where
  show game = show $ case logicalGrid game of
                       (Just grid) -> grid
                       Nothing -> emptyGrid 0 0

instance Show Square where
  show Empty = "_"
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

-- |A default game instance.
defaultGame :: Game
defaultGame = Game {_grid=defaultGrid, _piece=p, _pieceGen=ps}
  where
    (p:ps) = allPiecesAtTop

-- |Gets the logical grid with the current piece merged.
logicalGrid :: Game -> Maybe Grid
logicalGrid game = withPiece (game ^. piece) (game ^. grid)

-- |Fixes the current piece where it is and generates a new one.
fixPiece :: Game -> Game
fixPiece game = -- TODO: This is awful. Need a way to avoid the Maybe logic goign around.
  case logicalGrid game of
    (Just grid) -> Game {_grid=grid, _piece=newP, _pieceGen=ps}
  where
    (newP:ps) = game ^. pieceGen

-- |Steps the game forward by dropping the current piece.
-- |If it can't move, we fix the piece.
step :: Game -> Game
step game = if validateGame newGame then newGame else fixPiece game
  where
    -- TODO: NEED TO AVOID HITTING BOTTOM OF THE SCREEN
    newPiece = movePiece 0 (-1) $ game ^. piece
    newGame = game & piece .~ newPiece

-- Nothing if the piece can't be placed, otherwise the game itself.
validateGame :: Game -> Bool
validateGame game =
  case logicalGrid game of
    (Just _) -> True
    Nothing -> False

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
allPiecesAtTop = map (movePiece 5 20) $ cycle [rPiece, lPiece, linePiece, sPiece, squarePiece, zPiece]

-- |Moves a piece by the given amounts.
movePiece :: Int -> Int -> Piece -> Piece
movePiece x y (Piece cs) = Piece $ map (moveCoordinate x y) cs

-- |Moves a coordinate by the given amounts.
moveCoordinate :: Int -> Int -> Coordinate -> Coordinate
moveCoordinate x y (Coordinate (x', y')) = Coordinate (x + x', y + y')

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
  (Grid gs) <- withCoordinate (Coordinate (x, y-1)) (Grid gs)
  return $ Grid (g:gs)

-- |Sets the x value of the given row if possible.
setX :: Int -> Row -> Maybe Row
setX 0 (Row (Full:_)) = Nothing
setX 0 (Row (Empty:xs)) = Just $ Row (Full:xs)
setX n (Row (x:xs)) = do
  (Row xs) <- setX (n-1) (Row xs)
  return $ Row (x:xs)
