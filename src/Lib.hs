module Lib
  ( Game,
    Board,
    Position,
    Player(..),
    Move(..),
    defaultGame,
    makeMove,
    hasWinner,
    simulateGame,
  ) where

import Data.Sequence as S

-- Current player and board state
type Game = (Player, Board)

-- Board is a matrix of moves represented as a flat sequence.
-- Board position is 1-based, so valid positions are [1..9]
--
-- 1 2 3     X X O
-- 4 5 6 <=> O O X <=> [X,X,O,O,O,X,X,O,X]
-- 7 8 9     X O X
--
type Board = S.Seq Move

--instance Show Board where
--  show board = TODO

-- Board position is Int
type Position = Int

-- Two player turn based game
data Player
  = Player1 | Player2
  deriving (Eq, Ord, Show)

-- A move by a player on a board.
data Move
  = Nil | X | O
  deriving Eq

-- Maps moves to strings
instance Show Move where
  show Nil = "-"
  show X   = "X"
  show O   = "O"

-- Create an empty board with Player1 moving first.
defaultGame :: Game
defaultGame =
  (Player1, S.replicate (3 * 3) Nil)

-- Map players to move values
moveFor :: Player -> Move
moveFor player =
  if player == Player1 then X else O

-- Determine the player for the next turn.
nextTurn :: Player -> Player
nextTurn player =
  if player == Player1 then Player2 else Player1

-- Add a move to the board at a given position.
makeMove :: Game -> Position -> Game
makeMove (player, board) position =
  if isValidMove
    then (nextTurn player, updatedBoard)
    else (player, board)
  where
    idx = position - 1
    isValidMove = S.lookup idx board == Just Nil
    updatedBoard = S.update idx (moveFor player) board

-- A winning slice is 3 of the same moves in sequence
isWinningSlice :: S.Seq Move -> Bool
isWinningSlice slice =
  S.length slice == 3 && (all (== X) slice || all (== O) slice)

-- A slice is a sub-sequence of moves
getSlice :: Board -> [Position] -> S.Seq Move
getSlice board idxs =
  S.fromList $
    map (S.index board) idxs

-- Get all slices from the board that could contain a winning sequence of moves.
-- Note that indexes are used here, not positions, because this is a private function.
getSlices :: Board -> S.Seq (S.Seq Move)
getSlices board =
    S.fromList $ map (getSlice board) indexTable
  where
    indexTable =
      [ [0, 1, 2], [3, 4, 5], [6, 7, 8], -- rows
        [0, 3, 6], [1, 4, 7], [2, 5, 8], -- columns
        [0, 4, 8], [2, 4, 6]             -- diagonals
      ]

-- Determine whether a player has won.
hasWinner :: Board -> Bool
hasWinner board =
  any isWinningSlice (getSlices board)

-- Simulate game play given a sequence of positions.
simulateGame :: Game -> [Position] -> (Game, Bool)
simulateGame game [] = (game, False)
simulateGame game@(player, _) (position : rest) =
  let (nextPlayer, updatedBoard) = makeMove game position in
    if hasWinner updatedBoard
      then ((player, updatedBoard), True)
      else simulateGame (nextPlayer, updatedBoard) rest
