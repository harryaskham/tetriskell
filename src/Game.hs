{-# LANGUAGE TemplateHaskell #-}

module Game where

import Coordinate
import Piece
import Grid

import Data.List
import Control.Lens hiding (Empty)
import System.Random

-- |The representation of the game state.
data Game = Game { _grid :: Grid, _piece :: Piece, _pieceGen :: StdGen}

-- |The possible moves at any given time.
data Move = Left1 | Right1 | RotateCW | RotateCCW | Down1 | Drop deriving (Show, Bounded, Enum, Eq)

makeLenses ''Game

instance Show Game where
  show game = (show $ nextPiece game) ++ "\n" ++ (show $ logicalGridUnsafe (displayGame game))

-- |A game with the given random seed.
gameWithSeed :: StdGen -> Game
gameWithSeed seed = Game {_grid=defaultGrid, _piece=p, _pieceGen=g}
  where
    (p, g) = randomPieceAtTop seed

-- |Gets the logical grid with the current piece merged.
-- |Fails if the piece doesn't fit.
logicalGrid :: Game -> Maybe Grid
logicalGrid game = withPiece (game ^. piece) (game ^. grid)

-- |Gets the logical grid with the current piece merged.
logicalGridUnsafe :: Game -> Grid
logicalGridUnsafe game = withPieceUnsafe (game ^. piece) (game ^. grid)

-- |Gets the game for display.
displayGame :: Game -> Game
displayGame game = withGhostPiece game

-- |Gets a copy of the game with the ghost-piece placed.
-- |Use for display only.
withGhostPiece :: Game -> Game
withGhostPiece game = game & grid .~ (ghostGame ^. grid)
  where
    ghostGame = (fixPiece . makePieceBlack) $ moveFullyDown game

-- |Makes the current piece black.
makePieceBlack :: Game -> Game
makePieceBlack game = game & piece %~ makeBlack

-- |Gets the next piece to be dropped.
nextPiece :: Game -> Piece
nextPiece game = piece
  where
    (piece, _) = randomPieceAtTop $ game ^. pieceGen

-- |Fixes the current piece where it is and generates a new one.
fixPiece :: Game -> Game
fixPiece game = Game {_grid=logicalGridUnsafe game, _piece=p, _pieceGen=g}
  where
    (p, g) = randomPieceAtTop $ game ^. pieceGen

-- |Remove any completed rows.
flushCompleted :: Game -> Game
flushCompleted game = game & grid %~ flushGrid

-- |Steps the game forward by dropping the current piece.
-- |If it can't move, we fix the piece.
step :: Game -> Game
step game = flushCompleted $ guardGame (fixPiece game) newGame
  where
    newPiece = movePiece 0 (-1) $ game ^. piece
    newGame = game & piece .~ newPiece

-- |Is the game complete?
isComplete :: Game -> Bool
isComplete game = isGridComplete $ game ^. grid

-- |Either get the updated game if valid, or return the default.
guardGame :: Game -> Game -> Game
guardGame defaultGame game = 
  case logicalGrid game of
    (Just _) -> game
    Nothing -> defaultGame

-- |Move the current piece all the way down for ghost-piece purposes / drops.
moveFullyDown :: Game -> Game
moveFullyDown game = foldr (.) id (replicate 24 (move Down1)) $ game

-- |Apply the given move to the game if possible.
-- |If not possible, just returns the current game.
move :: Move -> Game -> Game 
move Left1 game = guardGame game $ game & piece %~ (movePiece (-1) 0)
move Right1 game = guardGame game $ game & piece %~ (movePiece 1 0)
move Down1 game = guardGame game $ game & piece %~ (movePiece 0 (-1))
move Drop game = flushCompleted . fixPiece $ moveFullyDown game
move RotateCW game = guardGame game $ game & piece %~ rotate CW
move RotateCCW game = guardGame game $ game & piece %~ rotate CCW

-- |Apply the given moves to the game in order.
applyMoves :: [Move] -> Game -> Game
applyMoves [] game = game
applyMoves (m:ms) game = applyMoves ms $ move m game
