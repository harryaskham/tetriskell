module Agent where

import System.Random
import Data.List
import Control.Lens
import Data.Maybe

import Game
import Grid

instance Random Move where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

randomMove :: StdGen -> (Move, StdGen)
randomMove g = random g

-- |TODO: Generate all possible futures (all rotations, all left/rights, all drops)
-- |Use this for a one-piece lookahead future (two piece using nextPiece)
-- |Use these to pick the best future score.
-- |Give back all the moves that got us there.

-- |Generates the moves that will generate all possible dropsites.
-- |Quite wasteful, generates dupes.
allMoves :: [[Move]]
allMoves = movesWithDrops
  where
    takeToN n = map take [0..n]
    sideMoves = (takeToN 10 <*> [repeat Left1]) ++ (takeToN 10 <*> [repeat Right1])
    rotations = (takeToN 2 <*> [repeat RotateCW]) ++ (takeToN 2 <*> [repeat RotateCCW])
    movesWithRotations = (map (++) rotations) <*> sideMoves
    movesWithDrops = map (++ [Drop]) movesWithRotations

-- |Apply moves with tracking.
applyMovesTracked :: [Move] -> Game -> (Game, [Move])
applyMovesTracked moves game = (applyMoves moves game, moves)

-- |Generate all games from here that have every combo of left, right, rotation and drop
-- |Keeps track of the moves that generated it.
allFutures :: Game -> [(Game, [Move])]
allFutures game = steppedGames
  where
    allFutureGames = (map applyMovesTracked allMoves) <*> [game]
    steppedGames = map (\(g, ms) -> (step g, ms)) allFutureGames

-- |Takes a future game and its moves, and looks one step into the future from there.
extendFutures :: (Game, [Move]) -> [(Game, [Move])]
extendFutures (game, moves) = map (\(g, ms) -> (g, moves ++ ms)) $ allFutures game

-- |Gets the best future and the moves that got us there.
bestFuture :: Game -> (Game, [Move])
bestFuture game = minimumBy compareCost $ futures
  where
    compareCost = (\(g1, _) (g2, _) -> compare (cost g1) (cost g2))
    futures = allFutures game -- >>= extendFutures

-- |Get the index of the first row that has no contents.
lowestEmptyRow :: Game -> Int
lowestEmptyRow game = fromMaybe 0 lowestEmptyRow
  where
    (Grid rows) = game ^. grid
    lowestEmptyRow = findIndex (== True) (map rowEmpty rows)

-- |Count the number of gaps per row.
rowGaps :: Row -> Int
rowGaps (Row (r:rs)) = length $ filter (\(a, b) -> a /= b) (zip (r:rs) rs)

-- |Count the number of vertical gaps in the grid
verticalGaps :: Grid -> Int
verticalGaps (Grid []) = 0
verticalGaps (Grid [_]) = 0
verticalGaps (Grid (r1:r2:rs)) = numGaps + (verticalGaps $ Grid rs)
  where
    (Row s1) = r1
    (Row s2) = r2
    numGaps = length $ filter (\(a, b) -> a/= b) (zip s1 s2)

-- |How many gaps in the entire game?
horizontalGaps :: Grid -> Int
horizontalGaps (Grid rows) = sum $ map rowGaps rows

-- |Assign a cost to a game.
cost :: Game -> Int
cost game = (20 * lowestEmptyRow game) +
            (1 * (horizontalGaps $ game ^. grid)) +
            (3 * (verticalGaps $ game ^. grid))
