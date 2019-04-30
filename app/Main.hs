module Main where

import Game

import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.IO

-- TODO:
-- Better input handling. We probably need separate forkIOs for display and for input, which means the Game itself would be an MVar.
-- Piece rotation
-- Line detection / clearing
-- Scoring
-- Correct failure (>20 lines)

-- |Clear the terminal screen.
clear :: IO ()
clear = putStr "\ESC[2J"

-- |Run a single step of the game, printing as we go.
-- |Spin-locks if the game won't proceed.
runStep :: [Move] -> Game -> IO (Maybe Game)
runStep moves game = do
  putStrLn $ show moves
  putStrLn $ show game
  return $ step moves game

-- |Run an entire game.
runGame :: Game -> IO ()
runGame game = do
  hSetBuffering stdin NoBuffering
  clear
  inputsMV <- newMVar []
  forkIO $ addInputs inputsMV
  threadDelay 100000
  inputs <- takeMVar inputsMV
  game <- runStep (catMaybes (map toMove inputs)) game
  case game of
    Just game -> runGame game
    Nothing -> putStrLn "Finished"

-- |Build up a list of chars given by getChar.
addInputs :: MVar [Char] -> IO ()
addInputs inputsMV = do
  c <- getChar
  inputs <- takeMVar inputsMV
  putMVar inputsMV (c:inputs)
  addInputs inputsMV

-- |Maps inputs to moves.
toMove :: Char -> Maybe Move
toMove 'j' = Just Left1
toMove 'l' = Just Right1
toMove 'k' = Just Down1
toMove _ = Nothing

main :: IO ()
main = runGame defaultGame
