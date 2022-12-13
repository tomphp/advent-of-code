module Day1 (run) where

import           Data.Bifunctor (first)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Day1.Elf (Elf)
import qualified Day1.Parser as Parser
import           Day1.Challenge (Results(..))
import qualified Day1.Challenge as Challenge
import           Text.Megaparsec.Error (errorBundlePretty)

run :: FilePath -> IO ()
run inputFilePath = do
    putStrLn "+++ Day 1 +++"
    elves <- loadElves inputFilePath
    case Challenge.getStats <$> elves of
        Right results -> printResults results
        Left e -> printError e

loadElves :: FilePath -> IO (Either Text [Elf])
loadElves inputFilePath = do
    input <- TIO.readFile inputFilePath
    let parsed = Parser.parseInput input
    return $ first (tshow . errorBundlePretty) parsed

printResults :: Results -> IO ()
printResults Results{numElves, maxCalories} = do
    TIO.putStrLn $ "Elves loaded: " <> tshow numElves
    TIO.putStrLn $ "Max Elf: " <> tshow maxCalories

printError :: Text -> IO ()
printError = TIO.putStrLn . tshow

tshow :: (Show a) => a -> Text
tshow = T.pack . show