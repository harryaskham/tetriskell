module Main where

import Game
import Agent

import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.IO
import System.IO.HiddenChar
import System.Random

-- TODO:
-- Better search (A* maybe)
-- Profiling
-- Scoring
-- Speed linked to level & score
-- Rotation correction, not blocking, better l-piece rotation

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
moveLoop :: MVar Game -> MVar [Move] -> IO ()
moveLoop gameMv movesMv = do
  moves <- takeMVar movesMv
  game <- takeMVar gameMv
  game <- return $ applyMoves moves game
  putMVar gameMv game
  putMVar movesMv []
  moveLoop gameMv movesMv

-- |Run the game loop.
gameLoop :: MVar Game -> IO ()
gameLoop gameMv = do
  game <- takeMVar gameMv
  if isComplete game then
    return ()
  else do
    putMVar gameMv $ step game
    threadDelay 1000000
    gameLoop gameMv

-- |Build up a list of input moves given by getChar.
getMoves :: MVar [Move] -> IO ()
getMoves movesMv = do
  hSetBuffering stdin NoBuffering
  c <- getHiddenChar
  case toMove c of
    Just c -> modifyMVar_ movesMv (\is -> return (c:is))
    Nothing -> return ()
  getMoves movesMv

-- |Executes the given movelist somewhat slowly.
executeAgentMoves :: [Move] -> MVar [Move] -> IO ()
executeAgentMoves [] _ = return ()
executeAgentMoves (m:ms) movesMv = do
  modifyMVar_ movesMv (return . (m:))
  threadDelay 50000
  executeAgentMoves ms movesMv

-- |Get the AI moves.
getAgentMoves :: MVar Game -> MVar [Move] -> IO ()
getAgentMoves gameMv movesMv = do
  moves <- withMVar gameMv (\g -> return $ bestDrop g)
  executeAgentMoves moves movesMv
  getAgentMoves gameMv movesMv

-- |Maps input to move.
toMove :: Char -> Maybe Move
toMove 'j' = Just Left1
toMove 'l' = Just Right1
toMove 'k' = Just Down1
toMove 'i' = Just RotateCCW
toMove 'u' = Just RotateCW
toMove ' ' = Just Drop
toMove _ = Nothing

-- |Generates the moveset for the cached input.
toMoves :: [Char] -> [Move]
toMoves = reverse . catMaybes . (map toMove)

defaultGame :: Game
defaultGame = gameWithSeed $ mkStdGen 42

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  seed <- getStdGen
  gameMv <- newMVar $ gameWithSeed seed
  movesMv <- newMVar []
  forkIO $ do printLoop gameMv
  forkIO $ do moveLoop gameMv movesMv
  forkIO $ do getMoves movesMv
  forkIO $ do getAgentMoves gameMv movesMv
  gameLoop gameMv
