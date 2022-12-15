module Day1.LogicSpec (spec) where

import Day1.Elf (Elf (Elf))
import Day1.Logic (Results (Results, maxCalories, numElves, top3Calories), getStats)
import Test.Hspec

spec :: Spec
spec = describe "Day1.Logic" $ do
  describe "getStats" $ do
    it "returns results for no elves" $ do
      getStats []
        `shouldBe` Results
          { numElves = 0,
            maxCalories = 0,
            top3Calories = 0
          }

    it "is the maximum total calories" $ do
      getStats [Elf [1, 2], Elf [3, 4], Elf [5], Elf [6]]
        `shouldBe` Results
          { numElves = 4,
            maxCalories = 7,
            top3Calories = 18
          }
