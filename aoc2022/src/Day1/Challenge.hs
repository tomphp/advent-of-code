module Day1.Challenge (Results (..), getStats) where

import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Day1.Elf (Calories, Elf)
import qualified Day1.Elf as Elf

data Results = Results
  { numElves :: Int,
    maxCalories :: Calories,
    top3Calories :: Calories
  }
  deriving (Eq, Show)

getStats :: [Elf] -> Results
getStats elves =
  Results
    { numElves = length elves,
      ..
    }
  where
    sortedCalories = reverse $ sort $ map Elf.totalCalories elves
    maxCalories = headOrElse 0 sortedCalories
    top3Calories = sum $ take 3 sortedCalories

headOrElse :: a -> [a] -> a
headOrElse def = fromMaybe def . listToMaybe