module Agent where

import System.Random
import Data.List
import Data.Tuple.HT
import Control.Lens
import Control.Applicative
import Control.Monad
import Data.Maybe
import Control.Parallel.Strategies

import Game
import Grid

-- |Generates all sublists of repetition of the given item.
repeatToN :: Int -> a -> [[a]]
repeatToN n a = replicate <$> [0..n] ?? a

-- |Generates the moves that will generate all possible dropsites, incl. holds.
allMoves :: [[Move]]
allMoves = nub movesWithHolds
  where
    sideMoves = repeatToN 5 Left1 ++ repeatToN 5 Right1
    rotations = repeatToN 2 RotateCW ++ repeatToN 2 RotateCCW
    movesWithRotations = (++) <$> rotations <*> sideMoves
    movesWithDrops = (++ [Drop]) <$> movesWithRotations
    movesWithHolds = movesWithDrops ++ ((Hold:) <$> movesWithDrops)

-- |Apply moves with tracking.
applyMovesTracked :: [Move] -> Game -> (Game, [Move])
applyMovesTracked moves game = (applyMoves moves game, moves)

-- |Generate all games from here that have every combo of left, right, rotation and drop
-- |Keeps track of the moves that generated it.
allFutures :: Game -> [(Game, [Move])]
allFutures game = steppedGames
  where
    allFutureGames = applyMovesTracked <$> allMoves <*> pure game
    steppedGames = mapFst step <$> allFutureGames

-- |Cull futures by taking only the top N
cullFutures :: Int -> [(Game, [Move])] -> [(Game, [Move])]
cullFutures n futures = (\(g, ms, _) -> (g, ms)) <$> culledCostedFutures
  where
    sortByCost = sortBy (\(_, _, c1) (_, _, c2) -> compare c1 c2)
    sortedFutures = sortByCost $ costedFuturesPar futures
    culledCostedFutures = take n sortedFutures

-- |Calculate the cost of each future.
costedFutures :: [(Game, [Move])] -> [(Game, [Move], Int)]
costedFutures = map (\(g, ms) -> (g, ms, cost g))

-- |Calculate the cost of each future.
costedFuturesPar :: [(Game, [Move])] -> [(Game, [Move], Int)]
costedFuturesPar = parMap rseq (\(g, ms) -> (g, ms, cost g))

-- |Takes a future game and its moves, and looks one step into the future from there.
-- |Ensures the move-chain is retained.
extendFutures :: (Game, [Move]) -> [(Game, [Move])]
extendFutures (game, origMoves) = mapSnd (origMoves++) <$> allFutures game

-- |Extends the future-list with culling.
extendWithCulling :: Int -> (Game, [Move]) -> [(Game, [Move])]
extendWithCulling n future = cullFutures n $ extendFutures future

-- |Runs multiple iterations of the culling extension.
-- |Uses the lookahead/culling global arguments.
extendFuturesN :: (Game, [Move]) -> [(Game, [Move])]
extendFuturesN = foldr (>=>) return $ replicate lookahead (extendWithCulling culling)
  where
    lookahead = 3  -- The number of moves into the future to consider
    culling = 3  -- The top N paths to consider

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
    moves = snd $ bestFuture game

-- |Modulate to change policies.
cost :: Game -> Int
cost = costA
-- cost = costB

-- |Assign a cost to a game. Plays basically perfect, but one row at a time.
costA :: Game -> Int
costA game = 0
            + 2 ^ lowestEmptyRow (game ^. grid)
            + 1 * horizontalGaps (game ^. grid)
            + 2 * verticalGaps (game ^. grid)

-- |Plays aesthetically with gaps, but avoids tunnels.
costB :: Game -> Int
costB game = 0
            + 3 ^ lowestEmptyRow (game ^. grid)
            + 2 * horizontalGaps (game ^. grid)
            + 2 ^ verticalGaps (game ^. grid)
            + 2 ^ numTunnels (game ^. grid)
