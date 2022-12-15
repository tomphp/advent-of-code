module Day2.Printer (printResults) where

import qualified Data.Text.IO as TIO
import Day2.Logic (Result (Result, firstScore, secondScore))
import Fmt

printResults :: Result -> IO ()
printResults Result {firstScore, secondScore} = do
  TIO.putStrLn ("First Score: " +|| firstScore ||+ "")
  TIO.putStrLn ("Second Score: " +|| secondScore ||+ "")
