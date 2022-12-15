module Day1.Printer (printResults) where

import qualified Data.Text.IO as TIO
import Day1.Logic (Results (..))
import Fmt

printResults :: Results -> IO ()
printResults Results {numElves, maxCalories, top3Calories} = do
  TIO.putStrLn $ "Elves loaded: " +| numElves |+ ""
  TIO.putStrLn $ "Max Elf: " +|| maxCalories ||+ ""
  TIO.putStrLn $ "Top 3: " +|| top3Calories ||+ ""
