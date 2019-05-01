module Main where

import Game

import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.IO

-- TODO:
-- Piece rotation
-- Line detection / clearing
-- Scoring
-- Correct failure (>20 lines)
-- Speed linked to level

-- |Clear the terminal screen.
clear :: IO ()
clear = putStr "\ESC[2J"

-- |Print the given game to the screen.
printGame :: MVar Game -> IO ()
printGame gameMv = do
  clear
  withMVar gameMv (putStrLn . show)
  threadDelay 16666  -- 60fps
  printGame gameMv

-- |Run the game loop.
runGame :: MVar Game -> MVar [Char] -> IO ()
runGame gameMv inputsMv = do
  game <- takeMVar gameMv
  inputs <- tryTakeMVar inputsMv
  game <- return $ step (catMaybes (map toMove $ fromMaybe [] inputs)) game
  threadDelay 100000 -- TODO: Change to affect game speed; should be level of game.
  case game of
    Just game -> do
      putMVar gameMv game
      runGame gameMv inputsMv
    Nothing -> putStrLn "Finished"

-- |Build up a list of chars given by getChar.
getInputs :: MVar [Char] -> IO ()
getInputs inputsMv = do
  hSetBuffering stdin NoBuffering
  c <- getChar
  modifyMVar_ inputsMv (\is -> do putStrLn (c:is); return (c:is))
  getInputs inputsMv

-- |Maps inputs to moves.
toMove :: Char -> Maybe Move
toMove 'j' = Just Left1
toMove 'l' = Just Right1
toMove 'k' = Just Down1
toMove _ = Nothing

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  gameMv <- newMVar defaultGame
  inputsMv <- newMVar []
  forkIO $ do printGame gameMv
  forkIO $ do runGame gameMv inputsMv
  getInputs inputsMv
