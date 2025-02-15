module Main (main) where

import Lib

-- Reference board positions
--
-- 1 2 3
-- 4 5 6
-- 7 8 9
--
-- Examples
--
-- player1: [1, 5, 9, 3, 7, 8, 4]
-- player2: [5, 1, 7, 3, 8, 2]
-- draw:    [1, 3, 2, 4, 6, 5, 9, 8, 7]
--
main :: IO ()
main = do
  let ((player, board), winner) = simulateGame defaultGame [1, 2, 3, 6, 5, 4, 7]
  if winner then do
    print board
    putStr "Winner = "
    print player
  else do
    print board
    putStrLn "Draw"
