{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Coordinate
import Piece

import Data.List
import Control.Lens hiding (Empty)

data Square = Empty | Full
data Row = Row [Square]
data Grid = Grid [Row]
data Game = Game { _grid :: Grid, _piece :: Piece, _pieceGen :: [Piece]}

makeLenses ''Game

-- TODO:
-- Piece rotation
-- User input
-- Line detection / clearing
-- Scoring
-- Correct failure (>20 lines)

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
fixPiece :: Game -> Maybe Game
fixPiece game = do
  grid <- logicalGrid game
  return Game {_grid=grid, _piece=newP, _pieceGen=ps}
  where
    (newP:ps) = game ^. pieceGen

-- |Steps the game forward by dropping the current piece.
-- |If it can't move, we fix the piece.
-- |TODO: Accept input / introduce AI or random moves
step :: Game -> Maybe Game
step game =
  if validateGame newGame then
    Just newGame
  else
    fixPiece game
  where
    newPiece = movePiece 0 (-1) $ game ^. piece
    newGame = game & piece .~ newPiece

-- | Step n times.
stepN :: Int -> Game -> Maybe Game
stepN 0 g = Just g
stepN n g = do
  g <- step g
  stepN (n-1) g

-- Nothing if the piece can't be placed, otherwise the game itself.
validateGame :: Game -> Bool
validateGame game =
  case logicalGrid game of
    (Just _) -> True
    Nothing -> False

-- |Places a piece on the grid.
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
