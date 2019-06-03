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
-- Piece preview on RHS, piece hold on LHS
-- Tracker for num lines to eval agents
-- Agent assesses drops with extra moves at the end e.g. L, R or rotation to slot into gaps
-- Scoring
-- Speed linked to level & score
-- Rotation correction, not blocking, better l-piece rotation
-- Piece hold

speedMod = 0.2  -- higher is slower; too low and you hit the decision timeout threshold
agentMoveDelay = round $ speedMod * 25000
fps60Delay = 16666
stepDelay = round $ speedMod * 100000

-- |Clear the terminal screen.
clear :: IO ()
clear = putStr "\ESC[2J"

-- |Print the given game to the screen.
printLoop :: MVar Game -> IO ()
printLoop gameMv = do
  clear
  withMVar gameMv (putStrLn . show)
  threadDelay fps60Delay
  printLoop gameMv

-- |Run the move-application loop.
moveLoop :: MVar Game -> MVar [Move] -> IO ()
moveLoop gameMv movesMv = do
  moves <- takeMVar movesMv
  game <- takeMVar gameMv
  putMVar gameMv $ applyMoves moves game
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
    threadDelay stepDelay
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
  threadDelay agentMoveDelay
  executeAgentMoves ms movesMv

-- |Get the AI moves.
getAgentMoves :: MVar Game -> MVar [Move] -> IO ()
getAgentMoves gameMv movesMv = do
  moves <- withMVar gameMv (\g -> return $ bestDrop g)
  --executeAgentMoves moves movesMv
  executeAgentMovesHack moves gameMv
  getAgentMoves gameMv movesMv

-- |SPEED HACK: Agent is able to modify the game directly instead of going via the moveLoop.
executeAgentMovesHack :: [Move] -> MVar Game -> IO ()
executeAgentMovesHack [] _ = return ()
executeAgentMovesHack (m:ms) gameMv = do
  modifyMVar_ gameMv $ return . applyMoves [m]
  threadDelay agentMoveDelay
  executeAgentMovesHack ms gameMv

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
  -- forkIO $ do moveLoop gameMv movesMv
  -- enable above to accept human input
  forkIO $ do getMoves movesMv
  forkIO $ do getAgentMoves gameMv movesMv
  gameLoop gameMv
