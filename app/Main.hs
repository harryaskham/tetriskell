module Main where

import Game
import Control.Concurrent

-- TODO:
-- User input
-- Piece rotation
-- Line detection / clearing
-- Scoring
-- Correct failure (>20 lines)

-- |Clear the terminal screen.
clear :: IO ()
clear = putStr "\ESC[2J"

-- |Run a single step of the game, printing as we go.
-- |Spin-locks if the game won't proceed.
runStep :: Game -> IO (Maybe Game)
runStep game = do
  putStrLn $ show game
  return $ step [Left1] game

-- |Run an entire game.
runGame :: Game -> IO ()
runGame game = do
  clear
  game <- runStep game
  threadDelay 100000
  case game of
    Just game -> runGame game
    Nothing -> putStrLn "Finished"

main :: IO ()
main = runGame defaultGame
