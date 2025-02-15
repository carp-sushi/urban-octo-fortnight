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
  let ((player, board), winner) = simulateGame initGame [1, 2, 3, 6, 5, 4, 7]
  if winner
    then do
      putStr $ showBoard board
      putStrLn $ "Winner = " <> show player
    else do
      putStr $ showBoard board
      putStrLn "Draw"
