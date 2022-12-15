module Day2.ParserSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Text as T
import Day2.Game (Game (Game), Move (Paper, Rock, Scissors), Result (Draw, Lose, Win))
import Day2.Parser (game, input, strategy, theRequiredResult, theirMove, yourMove)
import Day2.Strategy (Strategy (Strategy))
import Test.Hspec
import Test.QuickCheck
import qualified Text.Megaparsec as M

spec :: Spec
spec = describe "Day2.Parser" $ do
  describe "input" $ do
    it "returns a game at end of file" $ do
      M.runParser input "" "A X\nB Z\nC Y"
        `shouldBe` Right
          ( [ Game Rock Rock,
              Game Scissors Paper,
              Game Paper Scissors
            ],
            [ Strategy Rock Lose,
              Strategy Paper Win,
              Strategy Scissors Draw
            ]
          )

  describe "game" $ do
    it "returns a game at end of file" $ do
      M.runParser game "" "A X" `shouldBe` Right (Game Rock Rock)

    it "returns a game with newline" $ do
      M.runParser game "" "A X\n" `shouldBe` Right (Game Rock Rock)

    it "fails with trailing chars" $ do
      M.runParser game "" "A XQ" `shouldSatisfy` isLeft

  describe "strategy" $ do
    it "returns a strategy at end of file" $ do
      M.runParser strategy "" "A X" `shouldBe` Right (Strategy Rock Lose)

    it "returns a strategy with newline" $ do
      M.runParser strategy "" "A X\n" `shouldBe` Right (Strategy Rock Lose)

    it "fails with trailing chars" $ do
      M.runParser strategy "" "A XQ" `shouldSatisfy` isLeft

  describe "theirMove" $ do
    it "returns Rock for A" $ do
      M.runParser theirMove "" "A" `shouldBe` Right Rock

    it "returns Paper for B" $ do
      M.runParser theirMove "" "B" `shouldBe` Right Paper

    it "returns Scissors for C" $ do
      M.runParser theirMove "" "C" `shouldBe` Right Scissors

    it "fails for unknown chars" $
      forAll (charsExcept ['A', 'B', 'C']) $ \c ->
        M.runParser theirMove "" (T.pack [c]) `shouldSatisfy` isLeft

  describe "yourMove" $ do
    it "returns Rock for X" $ do
      M.runParser yourMove "" "X" `shouldBe` Right Rock

    it "returns Paper for Y" $ do
      M.runParser yourMove "" "Y" `shouldBe` Right Paper

    it "returns Scissors for Z" $ do
      M.runParser yourMove "" "Z" `shouldBe` Right Scissors

    it "fails for unknown chars" $
      forAll (charsExcept ['X', 'Y', 'Z']) $ \c ->
        M.runParser yourMove "" (T.pack [c]) `shouldSatisfy` isLeft

  describe "theRequiredResult" $ do
    it "returns Lose for X" $ do
      M.runParser theRequiredResult "" "X" `shouldBe` Right Lose

    it "returns Paper for Y" $ do
      M.runParser theRequiredResult "" "Y" `shouldBe` Right Draw

    it "returns Scissors for Z" $ do
      M.runParser theRequiredResult "" "Z" `shouldBe` Right Win

    it "fails for unknown chars" $
      forAll (charsExcept ['X', 'Y', 'Z']) $ \c ->
        M.runParser theRequiredResult "" (T.pack [c]) `shouldSatisfy` isLeft

charsExcept :: [Char] -> Gen Char
charsExcept exclude = suchThat arbitrary (not . (`elem` exclude))
