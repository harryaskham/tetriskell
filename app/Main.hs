module Main where

import Game

import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.IO
import System.IO.HiddenChar

-- TODO:
-- Piece rotation (define pieces in terms of a centrepoint and offsets from there)
-- Line detection / clearing on fixPiece
-- Scoring
-- Correct failure (>20 lines)
-- Speed linked to level
-- Random piece generation

-- |Clear the terminal screen.
clear :: IO ()
clear = putStr "\ESC[2J"

-- |Print the given game to the screen.
printLoop :: MVar Game -> IO ()
printLoop gameMv = do
  clear
  withMVar gameMv (putStrLn . show)
  threadDelay 16666  -- 60fps
  printLoop gameMv

-- |Run the move-application loop.
moveLoop :: MVar Game -> MVar [Char] -> IO ()
moveLoop gameMv inputsMv = do
  inputs <- takeMVar inputsMv
  game <- takeMVar gameMv
  game <- return $ applyMoves (toMoves inputs) game
  putMVar gameMv game
  putMVar inputsMv []
  moveLoop gameMv inputsMv
  
-- |Run the game loop.
gameLoop :: MVar Game -> IO ()
gameLoop gameMv = do
  game <- takeMVar gameMv
  case step game of
    Just game -> do
      putMVar gameMv game
      threadDelay 1000000 -- TODO: Change to affect game speed; should be level of game.
      gameLoop gameMv
    Nothing -> return ()

-- |Build up a list of chars given by getChar.
getInputs :: MVar [Char] -> IO ()
getInputs inputsMv = do
  hSetBuffering stdin NoBuffering
  c <- getHiddenChar
  modifyMVar_ inputsMv (\is -> return (c:is))
  getInputs inputsMv

-- |Maps input to move.
toMove :: Char -> Maybe Move
toMove 'j' = Just Left1
toMove 'l' = Just Right1
toMove 'k' = Just Down1
toMove 'i' = Just RotateCW
toMove 'u' = Just RotateCW
toMove ' ' = Just Drop
toMove _ = Nothing

-- |Generates the moveset for the cached input.
toMoves :: [Char] -> [Move]
toMoves = reverse . catMaybes . (map toMove)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  gameMv <- newMVar defaultGame
  inputsMv <- newMVar []
  forkIO $ do printLoop gameMv
  forkIO $ do gameLoop gameMv
  forkIO $ do moveLoop gameMv inputsMv
  getInputs inputsMv
