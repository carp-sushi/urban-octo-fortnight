import Lib

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import qualified Data.Sequence as S

-- Tests game init operation
spec_initGame :: Spec
spec_initGame =
  describe "initGame" $ do
    let (player, board) = initGame
    it "should return a board with the proper size" $ do
      S.length board `shouldBe` 9
    it "should return a board that contains no moves" $ do
      all (== Nil) board `shouldBe` True
    it "should choose Player1 to move first" $ do
      player `shouldBe` Player1

-- Tests game movement operation
spec_makeMove :: Spec
spec_makeMove =
  describe "makeMove" $ do
    let game = initGame
    it "should place a move in the middle of the board" $ do
      makeMove game 5 `shouldBe` (Player2, S.fromList [Nil, Nil, Nil, Nil, X, Nil, Nil, Nil, Nil])
    it "should make no changes when passed an invalid position" $ do
      makeMove game 99 `shouldBe` game

-- Tests winner detection
spec_isWinningBoard :: Spec
spec_isWinningBoard =
  describe "isWinningBoard" $ do
    it "should detect a winner" $ do
      isWinningBoard (S.fromList [X, O, X, O, X, O, X, Nil, Nil]) `shouldBe` True
    it "should NOT detect a winner on the initial board" $ do
      let (_, board) = initGame
      isWinningBoard board `shouldBe` False
    it "should NOT detect a winner on an invalid board" $ do
      isWinningBoard (S.fromList [X, X, X]) `shouldBe` False

-- Tests whether a board is out of moves
spec_outOfMoves :: Spec
spec_outOfMoves =
  describe "outOfMoves" $ do
    it "should return false for the initial board" $ do
      let (_, board) = initGame
      outOfMoves board `shouldBe` False
    it "should return true for a draw" $ do
      outOfMoves (S.fromList [O, X, O, O, X, X, X, O, X]) `shouldBe` True
    it "can return false for a win" $ do
      let board = S.fromList [X, O, X, O, X, O, X, Nil, Nil]
      isWinningBoard board `shouldBe` True
      outOfMoves board `shouldBe` False

-- Tests game simulation
spec_simulateGame :: Spec
spec_simulateGame =
  describe "simulateGame" $ do
    let game = initGame
    it "should simulate a player 1 win" $ do
      let expected = Right ((Player1, S.fromList [X, O, X, O, X, O, X, Nil, Nil]), Win)
      simulateGame game [1, 2, 3, 6, 5, 4, 7] `shouldBe` expected
    it "should simulate a player 2 win" $ do
      let expected = Right ((Player2, S.fromList [O, O, O, Nil, X, Nil, X, X, Nil]), Win)
      simulateGame game [5, 1, 7, 3, 8, 2] `shouldBe` expected
    it "should simulate a draw" $ do
      let expected = Right ((Player2, S.fromList [O, X, O, O, X, X, X, O, X]), Draw)
      simulateGame game [5, 1, 6, 4, 7, 3, 2, 8, 9] `shouldBe` expected
    it "should return an error with > 9 moves" $ do
      let expected = Left $ Error "simulation only supports a max of 9 moves"
      simulateGame game [1 .. 12] `shouldBe` expected

-- Collect all specs
allSpecs :: [Spec]
allSpecs =
  [ spec_initGame
  , spec_makeMove
  , spec_isWinningBoard
  , spec_outOfMoves
  , spec_simulateGame
  ]

-- Run tests
main :: IO ()
main = do
  specs <- fmap concat (mapM testSpecs allSpecs)
  defaultMain (testGroup "tic-tac-toe specs" specs)
