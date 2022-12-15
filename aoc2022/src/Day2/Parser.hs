module Day2.Parser (input, game, yourMove, theirMove) where

import Common.Parser (endOfLineOrFile)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Day2.Game (Game (Game, theirs, yours), Move (Paper, Rock, Scissors))
import Text.Megaparsec (Parsec, many, oneOf)
import Text.Megaparsec.Char (spaceChar)

theirsMap :: Map Char Move
theirsMap =
  Map.fromList
    [ ('A', Rock),
      ('B', Paper),
      ('C', Scissors)
    ]

yoursMap :: Map Char Move
yoursMap =
  Map.fromList
    [ ('X', Rock),
      ('Y', Paper),
      ('Z', Scissors)
    ]

input :: Parsec Void Text [Game]
input = many game

game :: Parsec Void Text Game
game = do
  theirs <- theirMove
  _ <- spaceChar
  yours <- yourMove
  endOfLineOrFile
  return $ Game {..}

theirMove :: Parsec Void Text Move
theirMove = charMapToType theirsMap

yourMove :: Parsec Void Text Move
yourMove = charMapToType yoursMap

charMapToType :: Map Char b -> Parsec Void Text b
charMapToType m = getType m <$> oneOf (Map.keys m)

getType :: Ord a => Map a b -> a -> b
getType m c = fromMaybe (error "should never happen") $ Map.lookup c m
