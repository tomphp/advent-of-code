module Day1.Elf (Elf (Elf), Calories, totalCalories) where

newtype Calories = Calories Integer
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

newtype Elf = Elf {foodItems :: [Calories]}
  deriving stock (Eq, Show)

totalCalories :: Elf -> Calories
totalCalories = sum . foodItems
