module Day1.ParserSpec (spec) where

import Data.Either (isLeft)
import Day1.Elf (Elf (Elf))
import Day1.Parser
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = describe "Day1.Parser" $ do
  describe "parseInput" $ do
    it "returns an empty list when given an empty string" $ do
      parseInput "" `shouldBe` Right []

    it "returns parsed elves" $ do
      parseInput "1000\n2000\n\n3000" `shouldBe` Right [Elf [1000, 2000], Elf [3000]]

  describe "foodItem" $ do
    it "parses a single food item" $ do
      M.runParser foodItem "" "104" `shouldBe` Right 104

    it "parses a single food item followed by newline" $ do
      M.runParser foodItem "" "905\n" `shouldBe` Right 905

    it "fails with invalid characters" $ do
      M.runParser foodItem "" "1a04" `shouldSatisfy` isLeft

  describe "sack" $ do
    it "parses multiple food items" $ do
      M.runParser sack "XXX" "104\n105" `shouldBe` Right [104, 105]

    it "parses up to a new line" $ do
      M.runParser sack "XXX" "104\n\n105" `shouldBe` Right [104]

    it "fails with invalid items" $ do
      M.runParser sack "XXX" "104\nxxx\n105" `shouldSatisfy` isLeft

  describe "elf" $ do
    it "parse a single elf" $ do
      M.runParser elf "XXX" "104\n105" `shouldBe` Right (Elf [104, 105])

    it "fails with invalid elf" $ do
      M.runParser elf "XXX" "104\nxxx\n105" `shouldSatisfy` isLeft
