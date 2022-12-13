module Day1.ElfSpec (spec) where

import Day1.Elf (Elf (Elf), totalCalories)
import Test.Hspec

spec :: Spec
spec = describe "Day1.Elf" $ do
  describe "totalCalories" $ do
    it "returns 0 when the elf has no items" $ do
      totalCalories (Elf [0]) `shouldBe` 0

    it "returns total of all items" $ do
      totalCalories (Elf [1, 2, 3]) `shouldBe` 6
