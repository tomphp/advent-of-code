module Day2.GameSpec (spec) where

import Day2.Game
  ( Game (Game, theirs, yours),
    Move (Paper, Rock, Scissors),
    Result (Draw, Lose, Win),
    result,
    scoreMove,
    scoreResult,
  )
import Test.Hspec

spec :: Spec
spec = describe "Day2.Game" $ do
  describe "scoreMove" $ do
    it "returns 1 for Rock" $ do
      scoreMove Rock `shouldBe` 1

    it "returns 2 for Paper" $ do
      scoreMove Paper `shouldBe` 2

    it "returns 3 for Scissors" $ do
      scoreMove Scissors `shouldBe` 3

  describe "scoreResult" $ do
    it "returns 0 for Lose" $ do
      scoreResult Lose `shouldBe` 0

    it "returns 3 for Draw" $ do
      scoreResult Draw `shouldBe` 3

    it "returns 6 for Win" $ do
      scoreResult Win `shouldBe` 6

  describe "result" $ do
    it "return Win for winning games" $ do
      result (Game {yours = Rock, theirs = Scissors}) `shouldBe` Win
      result (Game {yours = Scissors, theirs = Paper}) `shouldBe` Win
      result (Game {yours = Paper, theirs = Rock}) `shouldBe` Win

    it "return Draw for drawing games" $ do
      result (Game {yours = Rock, theirs = Rock}) `shouldBe` Draw
      result (Game {yours = Paper, theirs = Paper}) `shouldBe` Draw
      result (Game {yours = Scissors, theirs = Scissors}) `shouldBe` Draw

    it "return Lose for losing games" $ do
      result (Game {yours = Rock, theirs = Paper}) `shouldBe` Lose
      result (Game {yours = Paper, theirs = Scissors}) `shouldBe` Lose
      result (Game {yours = Scissors, theirs = Rock}) `shouldBe` Lose
