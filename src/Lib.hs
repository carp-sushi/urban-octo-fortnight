module Lib (
  Game,
  Board,
  Player (..),
  Move (..),
  SimStatus (..),
  initGame,
  makeMove,
  isWinningBoard,
  simulateGame,
  showBoard,
) where

import qualified Data.Sequence as S

-- Current player and board state
type Game = (Player, Board)

-- Board is a sequence of moves
type Board = S.Seq Move

-- Two player turn based game
data Player
  = Player1
  | Player2
  deriving (Eq, Ord, Show)

-- A move by a player on a board.
data Move
  = Nil
  | X
  | O
  deriving (Eq)

-- Maps moves to strings
instance Show Move where
  show Nil = "-"
  show X = "X"
  show O = "O"

-- Row size
rowSize :: Int
rowSize = 3

-- Represents the sequence length for a board.
boardSize :: Int
boardSize = rowSize * rowSize

-- Create a new board with Player1 moving first.
initGame :: Game
initGame =
  (Player1, S.replicate boardSize Nil)

-- Determine which player moves next.
nextPlayer :: Player -> Player
nextPlayer player =
  if player == Player1 then Player2 else Player1

-- Determine move for current player.
moveFor :: Player -> Move
moveFor player =
  if player == Player1 then X else O

-- Add a move to the board at a given position.
makeMove :: Game -> Int -> Game
makeMove (player, board) position =
  if isValidMove
    then (nextPlayer player, board')
    else (player, board)
  where
    idx = position - 1
    isValidMove = S.lookup idx board == Just Nil
    board' = S.update idx (moveFor player) board

-- A slice is a sub-sequence of moves that could represent a win.
getSlice :: Board -> [Int] -> S.Seq Move
getSlice board idxs =
  S.fromList $
    map (S.index board) idxs

-- Get all slices from the board that could contain a winning sequence of moves.
-- Note that indexes are used here, not positions.
getSlices :: Board -> S.Seq (S.Seq Move)
getSlices board =
  if S.length board == boardSize
    then S.fromList $ map (getSlice board) indexTable
    else S.empty
  where
    rows = [[0, 1, 2], [3, 4, 5], [6, 7, 8]]
    cols = [[0, 3, 6], [1, 4, 7], [2, 5, 8]]
    diag = [[0, 4, 8], [2, 4, 6]]
    indexTable = rows <> cols <> diag

-- Determine whether a board contains a winning sequence of moves.
isWinningBoard :: Board -> Bool
isWinningBoard board =
  any isWinningSlice (getSlices board)
  where
    isWinningSlice slice =
      S.length slice == rowSize && (all (== X) slice || all (== O) slice)

-- Use a status for simulation
data SimStatus
  = Win
  | Draw
  deriving (Eq, Ord, Show)

-- Simulate game play given a sequence of positions.
simulateGame :: Game -> [Int] -> (Game, SimStatus)
simulateGame game [] = (game, Draw)
simulateGame game@(player, _) (position : rest) =
  if isWinningBoard board'
    then ((player, board'), Win)
    else simulateGame (player', board') rest
  where
    (player', board') = makeMove game position

-- Render board as string
showBoard :: Board -> String
showBoard board =
  foldl (\acc row -> acc <> row <> "\n") mempty rows
  where
    rows = fmap showRow (S.chunksOf rowSize board)
    showRow = foldl (\acc move -> acc <> show move <> " ") mempty
