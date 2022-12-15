module Day2.Parser
  ( input,
    game,
    strategy,
    yourMove,
    theirMove,
    theRequiredResult,
  )
where

import Common.Parser (endOfLineOrFile)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Day2.Game
  ( Game (Game, theirs, yours),
    Move (Paper, Rock, Scissors),
    Result (Draw, Lose, Win),
  )
import Day2.Strategy (Strategy (..))
import Text.Megaparsec (Parsec, lookAhead, many, oneOf)
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

resultMap :: Map Char Result
resultMap =
  Map.fromList
    [ ('X', Lose),
      ('Y', Draw),
      ('Z', Win)
    ]

input :: Parsec Void Text ([Game], [Strategy])
input = do
  games <- lookAhead $ many game
  strategies <- many strategy
  return (games, strategies)

strategy :: Parsec Void Text Strategy
strategy = do
  theirs <- theirMove
  _ <- spaceChar
  requiredResult <- theRequiredResult
  endOfLineOrFile
  return $ Strategy {..}

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

theRequiredResult :: Parsec Void Text Result
theRequiredResult = charMapToType resultMap

charMapToType :: Map Char b -> Parsec Void Text b
charMapToType m = getType m <$> oneOf (Map.keys m)

getType :: Ord a => Map a b -> a -> b
getType m c = fromMaybe (error "should never happen") $ Map.lookup c m
