module Day2.GameSpec (spec) where

import Day2.Game
  ( Game (Game, theirs, yours),
    Move (Paper, Rock, Scissors),
    Result (Draw, Lose, Win),
    defeaterOf,
    loserTo,
    result,
  )
import Test.Hspec

spec :: Spec
spec = describe "Day2.Game" $ do
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

  describe "defeaterOf" $ do
    it "returns Paper for Rock" $
      defeaterOf Rock `shouldBe` Paper

    it "returns Rock for Scissors" $
      defeaterOf Scissors `shouldBe` Rock

    it "returns Scissors for Paper" $
      defeaterOf Paper `shouldBe` Scissors

  describe "loserOf" $ do
    it "returns Rock for Paper" $
      loserTo Paper `shouldBe` Rock

    it "returns Scissors for Rock" $
      loserTo Rock `shouldBe` Scissors

    it "returns Paper for Scissors" $
      loserTo Scissors `shouldBe` Paper
