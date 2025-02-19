module Main (main) where

import Lib

-- Reference board positions / view / inputs
--
-- 1 2 3     X O X
-- 4 5 6 <=> O X O <=> [1, 2, 3, 6, 5, 4, 7]
-- 7 8 9     X - -
--
-- Examples:
--   player1 win: [1, 5, 9, 3, 7, 8, 4]
--   player2 win: [5, 1, 7, 3, 8, 2]
--   draw:        [5, 1, 6, 4, 7, 3, 2, 8, 9]
--

-- Interactive game play on the command line.
playGame :: Game -> IO ()
playGame game@(player, board) = do
  putStrLn $ showBoard board <> "Enter move[1-9] for " <> show player <> ":"
  line <- getLine
  let position = read line :: Int
  let (player', board') = makeMove game position
  if isWinningBoard board'
    then do putStrLn $ showBoard board' <> "Winner = " <> show player
    else
      if Nil `notElem` board'
        then do
          putStrLn $ showBoard board' <> "Draw"
        else do
          playGame (player', board')

-- Run the game play simulation
runSimulation :: IO ()
runSimulation = do
  case simulateGame initGame [1, 2, 3, 6, 5, 4, 7] of
    Left (Error message) -> putStrLn message
    Right ((player, board), status) ->
      case status of
        Win -> do
          putStr $ showBoard board
          putStrLn $ "Winner = " <> show player
        Draw -> do
          putStr $ showBoard board
          putStrLn "Draw"
        Incomplete -> do
          putStr $ showBoard board
          putStrLn "Game incomplete"

main :: IO ()
main = do
  putStrLn "Board positions\n1 2 3\n4 5 6\n7 8 9\n"
  putStrLn "Starting game..."
  playGame initGame
