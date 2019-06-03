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
allMoves = nub movesWithDrops
  where
    takeToN n = map take [0..n]
    sideMoves = (takeToN 5 <*> [repeat Left1]) ++ (takeToN 5 <*> [repeat Right1])
    rotations = (takeToN 2 <*> [repeat RotateCW]) ++ (takeToN 2 <*> [repeat RotateCCW])
    movesWithRotations = map (++) rotations <*> sideMoves
    movesWithDrops = map (++ [Drop]) movesWithRotations

-- |Apply moves with tracking.
applyMovesTracked :: [Move] -> Game -> (Game, [Move])
applyMovesTracked moves game = (applyMoves moves game, moves)

-- |Generate all games from here that have every combo of left, right, rotation and drop
-- |Keeps track of the moves that generated it.
allFutures :: Game -> [(Game, [Move])]
allFutures game = steppedGames
  where
    allFutureGames = map applyMovesTracked allMoves <*> pure game
    steppedGames = map (\(g, ms) -> (step g, ms)) allFutureGames

-- |Cull futures by taking only the top N
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
extendWithCulling n future = cullFutures n $ extendFutures future

-- |Runs multiple iterations of the culling extension.
-- |Uses the lookahead/culling global arguments.
extendFuturesN :: (Game, [Move]) -> [(Game, [Move])]
extendFuturesN = foldr (>=>) return $ replicate lookahead (extendWithCulling culling)
  where
    lookahead = 4  -- The number of moves into the future to consider
    culling = 4  -- The top N paths to consider

-- |Gets the best future and the moves that got us there.
bestFuture :: Game -> (Game, [Move])
bestFuture game = minimumBy compareCost $ extendFuturesN (game, mempty)
  where
    compareCost (g1, _) (g2, _) = compare (cost g1) (cost g2)

-- |Gets the best set of moves up to the first Drop event.
-- |This means that each move has the fullest context.
bestDrop :: Game -> [Move]
bestDrop game = takeWhile (/= Drop) moves ++ [Drop]
  where
    (_, moves) = bestFuture game

-- |Assign a cost to a game.
cost :: Game -> Int
cost game = 0
            + 2 ^ lowestEmptyRow (game ^. grid)
            + 1 * horizontalGaps (game ^. grid)
            + 2 * verticalGaps (game ^. grid)
