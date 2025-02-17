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
    putStr $ showBoard board
    putStrLn $ "Enter move[1-9] for " <> show player <> ":"
    line <- getLine
    let position = read line :: Int
    let (player', board') = makeMove game position
    if isWinningBoard board' then do
      putStr $ showBoard board'
      putStrLn $ "Winner = " <> show player
    else if outOfMoves board' then do
      putStr $ showBoard board'
      putStrLn "Draw"
    else do
      playGame (player', board')

main :: IO ()
main = do
  putStrLn "Grid Positions"
  putStrLn "1 2 3\n4 5 6\n7 8 9\n"
  putStrLn "Starting game..."
  playGame initGame
