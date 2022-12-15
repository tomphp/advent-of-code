module Day2.StrategySpec (spec) where

import Day2.Game (Game (Game, theirs, yours), Move (Paper, Rock, Scissors), Result (Draw, Lose, Win))
import qualified Day2.Game as Game
import Day2.Strategy (Strategy (Strategy, requiredResult, theirs), toGame)
import Test.Hspec

spec :: Spec
spec = describe "Day2.Strategy" $ do
  describe "toGame" $ do
    it "returns winning games" $ do
      let requiredResult = Win
      toGame Strategy {theirs = Rock, ..}
        `shouldBe` Game {theirs = Rock, yours = Paper}
      toGame Strategy {theirs = Paper, ..}
        `shouldBe` Game {theirs = Paper, yours = Scissors}
      toGame Strategy {theirs = Scissors, ..}
        `shouldBe` Game {theirs = Scissors, yours = Rock}

    it "returns drawing games" $ do
      let requiredResult = Draw
      toGame Strategy {theirs = Rock, ..}
        `shouldBe` Game {theirs = Rock, yours = Rock}
      toGame Strategy {theirs = Paper, ..}
        `shouldBe` Game {theirs = Paper, yours = Paper}
      toGame Strategy {theirs = Scissors, ..}
        `shouldBe` Game {theirs = Scissors, yours = Scissors}

    it "returns losing games" $ do
      let requiredResult = Lose
      toGame Strategy {theirs = Rock, ..}
        `shouldBe` Game {theirs = Rock, yours = Scissors}
      toGame Strategy {theirs = Paper, ..}
        `shouldBe` Game {theirs = Paper, yours = Rock}
      toGame Strategy {theirs = Scissors, ..}
        `shouldBe` Game {theirs = Scissors, yours = Paper}
