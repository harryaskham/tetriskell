{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Coordinate
import Piece
import Grid

import Data.List
import Control.Lens hiding (Empty)

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
