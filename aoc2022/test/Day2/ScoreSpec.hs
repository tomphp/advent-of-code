module Day2.ScoreSpec (spec) where

import Day2.Game (Move (Paper, Rock, Scissors), Result (Draw, Lose, Win))
import Day2.Score (scoreMove, scoreResult)
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
