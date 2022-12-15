module Main (main) where

import Common.Runner (Day, runDay)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Day1
import qualified Day2
import Fmt
import qualified System.Environment as SE
import Text.Read (readMaybe)

days :: Map Int Day
days =
  Map.fromList
    [ (1, Day1.day),
      (2, Day2.day)
    ]

main :: IO ()
main =
  runExceptT main' >>= \case
    Right _ -> return ()
    Left err -> TIO.putStrLn ("Error: " +| err |+ "")

main' :: ExceptT Text IO ()
main' = do
  dayNumber <- getArgs
  day <- getDay dayNumber
  liftIO $ TIO.putStrLn ("+++ Day " +| dayNumber |+ " +++")
  let path = inputPath dayNumber
  liftIO $ TIO.putStrLn ("Reading: " +| path |+ "")
  content <- liftIO $ TIO.readFile path
  runDay day content

getArgs :: (MonadIO m, MonadError Text m) => m Int
getArgs = do
  args <- liftIO SE.getArgs
  parseArgs args

parseArgs :: (MonadError Text m) => [String] -> m Int
parseArgs [] = throwError "You must provided the day number"
parseArgs [day] = case readMaybe day of
  Just dayNumber -> return dayNumber
  Nothing -> throwError "Day must be a number"
parseArgs _ = throwError "You must pass exactly one argument"

inputPath :: Int -> FilePath
inputPath day = "data/day" +| day |+ ".txt"

getDay :: (MonadError Text m) => Int -> m Day
getDay number =
  case Map.lookup number days of
    Just day -> return day
    Nothing -> throwError ("Day " +| number |+ " does not exist")
