module Agent where

import System.Random

import Game

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

highestFilledRow :: Game -> Int
highestFilledRow _ = 0 -- TODO

-- |Assign a score to a game.
score :: Game -> Int
score game = highestFilledRow game
