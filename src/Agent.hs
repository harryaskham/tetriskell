module Agent where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Parallel.Strategies
import Data.List
import Data.Maybe
import Data.Tuple.HT
import System.Random

import Game
import Grid
import Utils

-- |Type alias representing a given future game and the moves to get there.
type Future = (Game, [Move])

type Cost = Int

-- |A future, with its cost included.
type CostedFuture = (Game, [Move], Cost)

-- |Generates the moves that will generate all possible dropsites, incl. holds.
allMoves :: [[Move]]
allMoves = nub movesWithHolds
  where
    sideMoves = repeatToN 5 Left1 ++ repeatToN 5 Right1
    rotations = repeatToN 2 RotateCW ++ repeatToN 2 RotateCCW
    movesWithRotations = (++) <$> rotations <*> sideMoves
    movesWithDrops = (++ [Drop]) <$> movesWithRotations
    movesWithHolds = movesWithDrops ++ ((Hold :) <$> movesWithDrops)

-- |Apply moves with tracking.
applyMovesTracked :: [Move] -> Game -> Future
applyMovesTracked moves game = (applyMoves moves game, moves)

-- |Generate all games from here that have every combo of left, right, rotation and drop
-- |Keeps track of the moves that generated it.
allFutures :: Game -> [Future]
allFutures game = mapFst step <$> (applyMovesTracked <$> allMoves <*> pure game)

-- |Cull futures by taking only the top N
cullFutures :: Int -> [Future] -> [Future]
cullFutures n futures = fst2of3 <$> culledCostedFutures
  where
    sortByCost = sortBy compareThd
    sortedFutures = sortByCost $ costedFuturesPar futures
    culledCostedFutures = take n sortedFutures

-- |Augments a future tuple with its cost
addCost :: Future -> CostedFuture
addCost (g, ms) = (g, ms, cost g)

-- |Calculate the cost of each future.
costedFutures :: [Future] -> [CostedFuture]
costedFutures = fmap addCost

-- |Calculate the cost of each future.
costedFuturesPar :: [Future] -> [CostedFuture]
costedFuturesPar = parMap rseq addCost

-- |Takes a future game and its moves, and looks one step into the future from there.
-- |Ensures the move-chain is retained.
extendFutures :: Future -> [Future]
extendFutures (game, origMoves) = (origMoves ++) <$$> allFutures game

-- |Extends the future-list with culling.
extendWithCulling :: Int -> Future -> [Future]
extendWithCulling n = cullFutures n . extendFutures

-- |Runs multiple iterations of the culling extension.
-- |Uses the lookahead/culling global arguments.
extendFuturesN :: Future -> [Future]
extendFuturesN =
  foldr (>=>) return $ replicate lookahead (extendWithCulling culling)
  where
    lookahead = 3 -- The number of moves into the future to consider
    culling = 3 -- The top N paths to consider

-- |Gets the best future and the moves that got us there.
bestFuture :: Game -> Future
bestFuture game = minimumBy compareCost $ extendFuturesN (game, mempty)
  where
    compareCost (g1, _) (g2, _) = compare (cost g1) (cost g2)

-- |Gets the best move sequence we know about so far.
-- |Note that this might be invalidated by extra lookahead so we often cull to Drop.
bestMoves :: Game -> [Move]
bestMoves = snd . bestFuture

-- |Gets the best set of moves up to the first Drop event.
-- |This means that each move has the fullest context.
bestDrop :: Game -> [Move]
bestDrop = takeWhileInclusive (/= Drop) . bestMoves

-- |Modulate to change policies.
cost :: Game -> Cost
cost = costA

-- cost = costB
-- |Assign a cost to a game. Plays basically perfect, but one row at a time.
costA :: Game -> Cost
costA game =
  2 ^ lowestEmptyRow (game ^. grid) + 1 * horizontalGaps (game ^. grid) +
  2 * verticalGaps (game ^. grid)

-- |Plays aesthetically with gaps, but avoids tunnels.
costB :: Game -> Cost
costB game =
  3 ^ lowestEmptyRow (game ^. grid) + 2 * horizontalGaps (game ^. grid) +
  2 ^ verticalGaps (game ^. grid) +
  2 ^ numTunnels (game ^. grid)
