module Day2.Logic (Result (..), calculateScore) where

import Day2.Game (Game)
import Day2.Score (Score)
import qualified Day2.Score as Score
import Day2.Strategy (Strategy)
import qualified Day2.Strategy as Strategy

data Result = Result
  { firstScore :: Score,
    secondScore :: Score
  }

calculateScore :: ([Game], [Strategy]) -> Result
calculateScore (games, strategies) =
  Result
    { firstScore = scoreGames games,
      secondScore = scoreGames $ Strategy.toGame <$> strategies
    }

scoreGames :: [Game] -> Score
scoreGames = sum . fmap Score.scoreGame
