module Day1.ChallengeSpec (spec) where

import Test.Hspec

import Day1.Challenge (Results(Results, numElves, maxCalories), getStats)
import Day1.Elf (Elf(Elf))

spec :: Spec
spec = describe "Day1.Challenge" $ do
    describe "getStats" $ do
        it "returns results for no elves" $ do
            getStats [] `shouldBe` Results { numElves = 0, maxCalories = 0 }

        it "is the maximum total calories" $ do
            getStats [Elf [1, 2], Elf [3, 4], Elf [5]] `shouldBe` Results { numElves = 3, maxCalories = 7 }
