module Day1.Challenge (Results (..), getStats) where

import Day1.Elf (Calories, Elf)
import qualified Day1.Elf as Elf

data Results = Results
  { numElves :: Int,
    maxCalories :: Calories
  }
  deriving (Eq, Show)

getStats :: [Elf] -> Results
getStats elves =
  Results
    { numElves = length elves,
      maxCalories = maxCarrier elves
    }

maxCarrier :: [Elf] -> Calories
maxCarrier [] = 0
maxCarrier elves = maximum (Elf.totalCalories <$> elves)
