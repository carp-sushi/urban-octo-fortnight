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
main :: IO ()
main = do
  putStrLn "Board positions\n1 2 3\n4 5 6\n7 8 9\n"
  putStrLn "Starting game..."
  playGame initGame

-- Interactive game play on the command line.
playGame :: Game -> IO ()
playGame game@(player, board) = do
  putStrLn $ showBoard board <> "Enter move[1-9] for " <> show player <> ":"
  line <- getLine
  let position = read line
  let (player', board') = makeMove game position
  case (isWinningBoard board', outOfMoves board') of
    (False, False) -> playGame (player', board')
    (False, True) -> putStrLn $ showBoard board' <> "Draw"
    (True, _) -> putStrLn $ showBoard board' <> "Winner = " <> show player

-- Run the game play simulation using a sequence of moves.
runSimulation :: [Int] -> IO ()
runSimulation positions = do
  case simulateGame initGame positions of
    Left (Error message) ->
      putStrLn message
    Right ((player, board), status) ->
      putStr (showBoard board) >> printResult status player

-- Print simulation result from output status
printResult :: SimStatus -> Player -> IO ()
printResult Win player = putStrLn $ "Winner = " <> show player
printResult Draw _ = putStrLn "Draw"
printResult Incomplete _ = putStrLn "Incomplete"
