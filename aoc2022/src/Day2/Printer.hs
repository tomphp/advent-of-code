module Day2.Printer (printResults) where

import qualified Data.Text.IO as TIO
import Day2.Logic (Result (Result, score))
import Fmt

printResults :: Result -> IO ()
printResults Result {score} = do
  TIO.putStrLn ("Total Score: " +|| score ||+ "")
