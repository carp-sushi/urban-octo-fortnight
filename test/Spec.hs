import Lib

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import qualified Data.Sequence as S

-- Tests game init operation
spec_init :: Spec
spec_init =
  describe "initGame" $ do
    let (player, board) = initGame
    it "should return a board with the proper size" $ do
      S.length board `shouldBe` 9
    it "should return a board that contains no moves" $ do
      all (== Nil) board `shouldBe` True
    it "should choose Player1 to move first" $ do
      player `shouldBe` Player1

-- Tests game movement operation
spec_move :: Spec
spec_move =
  describe "makeMove" $ do
    let game = initGame
    it "should place a move in the middle of the board" $ do
      makeMove game 5 `shouldBe` (Player2, S.fromList [Nil, Nil, Nil, Nil, X, Nil, Nil, Nil, Nil])
    it "should make no changes when passed an invalid position" $ do
      makeMove game 99 `shouldBe` game

-- Tests winner detection operation
spec_winner :: Spec
spec_winner =
  describe "isWinningBoard" $ do
    it "should detect a winner" $ do
      isWinningBoard (S.fromList [X, O, X, O, X, O, X, Nil, Nil]) `shouldBe` True
    it "should NOT detect a winner on the initial board" $ do
      let (_, board) = initGame
      isWinningBoard board `shouldBe` False
    it "should NOT detect a winner on an invalid board" $ do
      isWinningBoard (S.fromList [X, X, X]) `shouldBe` False

-- Tests game simulation operation
spec_simulation :: Spec
spec_simulation =
  describe "simulateGame" $ do
    let game = initGame
    it "should simulate a player 1 win" $ do
      let ((player, board), status) = simulateGame game [1, 2, 3, 6, 5, 4, 7]
      status `shouldBe` Win
      player `shouldBe` Player1
      board `shouldBe` S.fromList [X, O, X, O, X, O, X, Nil, Nil]
    it "should simulate a player 2 win" $ do
      let ((player, board), status) = simulateGame game [5, 1, 7, 3, 8, 2]
      status `shouldBe` Win
      player `shouldBe` Player2
      board `shouldBe` S.fromList [O, O, O, Nil, X, Nil, X, X, Nil]
    it "should simulate a draw" $ do
      let ((_, board), status) = simulateGame game [5, 1, 6, 4, 7, 3, 2, 8, 9]
      status `shouldBe` Draw
      board `shouldBe` S.fromList [O, X, O, O, X, X, X, O, X]

-- Collect all specs
allSpecs :: [Spec]
allSpecs =
  [ spec_init
  , spec_move
  , spec_winner
  , spec_simulation
  ]

-- Run tests
main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs allSpecs
  defaultMain (testGroup "tic-tac-toe specs" specs)
