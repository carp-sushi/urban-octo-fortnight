import Lib

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import Data.Sequence as S

-- Tests game init operation
spec_init_ops :: Spec
spec_init_ops =
  describe "initGame" $ do
    let (player, board) = initGame
    it "initial board should have the proper size" $ do
      S.length board `shouldBe` 9
    it "initial board should contain no moves" $ do
      all (== Nil) board `shouldBe` True
    it "player one should go first" $ do
      player `shouldBe` Player1

-- Tests game movement operation
spec_move_ops :: Spec
spec_move_ops =
  describe "makeMove" $ do
    let game = initGame
    it "should place a move in the middle of the board" $ do
      makeMove game 5 `shouldBe` (Player2, S.fromList [Nil, Nil, Nil, Nil, X, Nil, Nil, Nil, Nil])
    it "move with invalid position should make no changes" $ do
      makeMove game 99 `shouldBe` game

-- Tests winner detection operation
spec_winner_ops :: Spec
spec_winner_ops =
  describe "isWinningBoard" $ do
    let (_, board) = initGame
    it "should detect a valid winner" $ do
      isWinningBoard (S.fromList [X, O, X, O, X, O, X, Nil, Nil]) `shouldBe` True
    it "initial board shouldn't contain a winner" $ do
      isWinningBoard board `shouldBe` False
    it "invalid board shouldn't contain a winner" $ do
      isWinningBoard (S.fromList [X, X, X]) `shouldBe` False

-- Tests game simulation operation
spec_simulation_ops :: Spec
spec_simulation_ops =
  describe "simulateGame" $ do
    let game = initGame
    it "should simulate a player 1 win" $ do
      let ((player, board), isWinner) = simulateGame game [1, 2, 3, 6, 5, 4, 7]
      player `shouldBe` Player1
      board `shouldBe` S.fromList [X, O, X, O, X, O, X, Nil, Nil]
      isWinner `shouldBe` True
    it "should simulate a player 2 win" $ do
      let ((player, board), isWinner) = simulateGame game [5, 1, 7, 3, 8, 2]
      player `shouldBe` Player2
      board `shouldBe` S.fromList [O, O, O, Nil, X, Nil, X, X, Nil]
      isWinner `shouldBe` True
    it "should simulate a draw" $ do
      let ((_, board), isWinner) = simulateGame game [5, 1, 6, 4, 7, 3, 2, 8, 9]
      board `shouldBe` S.fromList [O, X, O, O, X, X, X, O, X]
      isWinner `shouldBe` False

-- Collect all specs
allSpecs :: [Spec]
allSpecs =
  [ spec_init_ops
  , spec_move_ops
  , spec_winner_ops
  , spec_simulation_ops
  ]

-- Run tests
main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs allSpecs
  defaultMain (testGroup "Game specs" specs)
