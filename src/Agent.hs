module Agent where

import System.Random
import Data.List
import Control.Lens
import Control.Monad
import Data.Maybe

import Game
import Grid

-- |Generates the moves that will generate all possible dropsites.
allMoves :: [[Move]]
allMoves = movesWithDrops
  where
    takeToN n = map take [0..n]
    sideMoves = (takeToN 5 <*> [repeat Left1]) ++ (takeToN 5 <*> [repeat Right1])
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
    allFutureGames = (map applyMovesTracked allMoves) <*> pure game
    steppedGames = map (\(g, ms) -> (step g, ms)) allFutureGames

-- |Cull futures by dropping the top N
cullFutures :: Int -> [(Game, [Move])] -> [(Game, [Move])]
cullFutures n futures = map (\(g, ms, _) -> (g, ms)) $ take n sortedFutures
  where
    costedFutures = map (\(g, ms) -> (g, ms, cost g)) futures
    sortedFutures = sortBy (\(_, _, c1) (_, _, c2) -> compare c1 c2) costedFutures

-- |Takes a future game and its moves, and looks one step into the future from there.
-- |Ensures the move-chain is retained.
extendFutures :: (Game, [Move]) -> [(Game, [Move])]
extendFutures (game, origMoves) = map (\(g, newMoves) -> (g, origMoves ++ newMoves)) $ allFutures game

-- |Extends the future-list with culling.
extendWithCulling :: Int -> (Game, [Move]) -> [(Game, [Move])]
extendWithCulling n future = cullFutures n (extendFutures future)

-- |Gets the best future and the moves that got us there.
bestFuture :: Game -> (Game, [Move])
bestFuture game = minimumBy compareCost $ futures
  where
    compareCost = (\(g1, _) (g2, _) -> compare (cost g1) (cost g2))
    present = (game, mempty)
    extend = extendWithCulling 3  -- The top N paths to consider
    extendN n = foldr (>=>) return (replicate n extend)
    futures = extendN 3 present  -- The number of moves into the future to consider

-- |Gets the best set of moves up to the first Drop event.
-- |This means that each move has the fullest context.
bestDrop :: Game -> [Move]
bestDrop game = takeWhile (\m -> m /= Drop) moves ++ [Drop]
  where
    (_, moves) = bestFuture game

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
cost game = (2 ^ lowestEmptyRow game) +
            (1 * (horizontalGaps $ game ^. grid)) +
            (2 * (verticalGaps $ game ^. grid))
