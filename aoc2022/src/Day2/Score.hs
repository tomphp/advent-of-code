module Day2.Score
  ( Score,
    scoreGame,
    scoreMove,
    scoreResult,
  )
where

import Day2.Game (Game (Game, yours), Move, Result (Draw, Lose, Win), result)

newtype Score = Score Int
  deriving stock (Eq, Show)
  deriving newtype (Num)

scoreGame :: Game -> Score
scoreGame game@(Game {yours}) = scoreResult (result game) + scoreMove yours

scoreMove :: Move -> Score
scoreMove = Score . succ . fromEnum

scoreResult :: Result -> Score
scoreResult Lose = 0
scoreResult Draw = 3
scoreResult Win = 6
