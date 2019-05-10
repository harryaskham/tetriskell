{-# LANGUAGE TemplateHaskell #-}

module Game where

import Coordinate
import Piece
import Grid

import Data.List
import Control.Lens hiding (Empty)
import Control.Monad.Loops
import System.Random

-- |The representation of the game state.
data Game = Game { _grid :: Grid, _piece :: Piece, _pieceGen :: StdGen}

-- |The possible moves at any given time.
data Move = Left1 | Right1 | RotateCW | RotateCCW | Down1 | Drop deriving (Show)

makeLenses ''Game

instance Show Game where
  show game = show $ case logicalGrid (withGhostPiece game) of
                       (Just grid) -> grid
                       Nothing -> emptyGrid 0 0

-- |A default game instance.
defaultGame :: Game
defaultGame = Game {_grid=defaultGrid, _piece=p, _pieceGen=g}
  where
    (p, g) = randomPieceAtTop $ mkStdGen 42

-- |Gets the logical grid with the current piece merged.
logicalGrid :: Game -> Maybe Grid
logicalGrid game = withPiece (game ^. piece) (game ^. grid)

-- |Gets a copy of the game with the ghost-piece placed.
-- |Use for display only.
withGhostPiece :: Game -> Game
withGhostPiece game = game & grid .~ (newGame ^. grid)
  where
    newGame = case move Drop game of
                Just game -> case fixPiece game of
                               Just game -> game
                               Nothing -> error "As below"
                Nothing -> error "Fix me by using Maybe more sensibly"

-- |Fixes the current piece where it is and generates a new one.
-- |Nothing if this piece kills the game.
fixPiece :: Game -> Maybe Game
fixPiece game = do
  grid <- logicalGrid game
  return Game {_grid=grid, _piece=p, _pieceGen=g}
  where
    (p, g) = randomPieceAtTop $ game ^. pieceGen

-- |Remove any completed rows.
flushCompleted :: Game -> Game
flushCompleted game = game & grid %~ flushGrid

-- |Steps the game forward by dropping the current piece.
-- |If it can't move, we fix the piece.
step :: Game -> Maybe Game
step game = do
  game <- if validateGame newGame then
    Just newGame
  else
    fixPiece game
  return $ flushCompleted game
  where
    newPiece = movePiece 0 (-1) $ game ^. piece
    newGame = game & piece .~ newPiece

-- Is the state of the game valid?
validateGame :: Game -> Bool
validateGame game =
  case logicalGrid game of
    (Just _) -> True
    Nothing -> False

-- |Only pass the game through if it's valid
guardGame :: Game -> Maybe Game
guardGame game = 
  case logicalGrid game of
    (Just _) -> Just game
    Nothing -> Nothing

-- |Apply the given move to the game if possible.
move :: Move -> Game -> Maybe Game 
move Left1 game = guardGame $ game & piece %~ (movePiece (-1) 0)
move Right1 game = guardGame $ game & piece %~ (movePiece 1 0)
move Down1 game = guardGame $ game & piece %~ (movePiece 0 (-1))
move Drop game =
  case move Down1 game of
    Just game -> move Drop game
    Nothing -> Just game
move RotateCW game = guardGame $ game & piece %~ rotate CW
move RotateCCW game = guardGame $ game & piece %~ rotate CCW

-- |Apply the given moves to the game in order, ignoring failures.
applyMoves :: [Move] -> Game -> Game
applyMoves [] game = game
applyMoves (m:ms) game =
  case move m game of
    Just newGame -> applyMoves ms newGame
    Nothing -> applyMoves ms game
