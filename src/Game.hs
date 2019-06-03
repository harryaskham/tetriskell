{-# LANGUAGE TemplateHaskell #-}

module Game where

import Coordinate
import Piece
import Grid

import Data.List
import Data.List.Split
import Data.Maybe
import Control.Lens hiding (Empty)
import System.Random

-- |The representation of the game state.
data Game = Game { _grid :: Grid, _piece :: Piece, _pieceGen :: StdGen, _score :: Int, _heldPiece :: Maybe Piece }

-- |The possible moves at any given time.
data Move = Left1 | Right1 | RotateCW | RotateCCW | Down1 | Drop | Hold deriving (Show, Bounded, Enum, Eq)

makeLenses ''Game

instance Show Game where
  show game = intercalate "\n" allLines
    where
      gameLines = splitOn "\n" $ show $ logicalGridUnsafe (displayGame game)
      nextPieceLines = linesForPiece (Just $ nextPiece game) "next"
      heldPieceLines = linesForPiece (game ^. heldPiece) "held"
      scoreLine = "score: " ++ show (game ^. score)
      allLines = scoreLine : "\n" : zipWith (++) gameLines (nextPieceLines ++ [""] ++ heldPieceLines ++ repeat "")

-- |Get the display lines for a given piece on the RHS of the grid.
linesForPiece :: Maybe Piece -> String -> [String]
linesForPiece piece header = map (' ':) $ splitOn "\n" (show $ fromMaybe emptyPiece piece) ++ [header]

-- |A game with the given random seed.
gameWithSeed :: StdGen -> Game
gameWithSeed seed = Game {_grid=defaultGrid, _piece=p, _pieceGen=g, _score=0, _heldPiece=Nothing}
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
displayGame = withGhostPiece

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
fixPiece game = withNextPiece game & grid .~ logicalGridUnsafe game

-- |Abandons the current piece and gets the next one.
withNextPiece :: Game -> Game
withNextPiece game = Game {_grid=game ^. grid, _piece=p, _pieceGen=g, _score=game ^. score, _heldPiece=game ^. heldPiece}
  where
    (p, g) = randomPieceAtTop $ game ^. pieceGen

-- |Remove any completed rows, updating the score.
flushCompleted :: Game -> Game
flushCompleted game = flushed & score %~ (+linesCleared)
  where
    flushed = game & grid %~ flushGrid
    linesCleared = numPopulatedRows (game ^. grid) - numPopulatedRows (flushed ^. grid)

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
moveFullyDown = foldr (.) id (replicate 24 (move Down1))

-- |Apply the given move to the game if possible.
-- |If not possible, just returns the current game.
move :: Move -> Game -> Game 
move Left1 game = guardGame game $ game & piece %~ movePiece (-1) 0
move Right1 game = guardGame game $ game & piece %~ movePiece 1 0
move Down1 game = guardGame game $ game & piece %~ movePiece 0 (-1)
move Drop game = flushCompleted . fixPiece $ moveFullyDown game
move RotateCW game = guardGame game $ game & piece %~ rotate CW
move RotateCCW game = guardGame game $ game & piece %~ rotate CCW
move Hold game =
  let pieceToHold = pieceAtTop $ game ^. piece in
    case game ^. heldPiece of
      Just held -> (game & piece .~ held) & heldPiece ?~ pieceToHold
      Nothing -> withNextPiece game & heldPiece ?~ pieceToHold

-- |Apply the given moves to the game in order.
applyMoves :: [Move] -> Game -> Game
applyMoves ms game = foldl (flip move) game ms
