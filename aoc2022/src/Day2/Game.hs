module Day2.Game
  ( Move (..),
    Result (..),
    Game (..),
    Score,
    result,
    scoreGame,
    scoreMove,
    scoreResult,
  )
where

data Move = Rock | Paper | Scissors
  deriving stock (Bounded, Eq, Enum, Show)

data Game = Game {yours :: Move, theirs :: Move}
  deriving stock (Eq, Show)

data Result = Win | Draw | Lose
  deriving stock (Eq, Show)

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

result :: Game -> Result
result Game {..}
  | yours == theirs = Draw
  | defeaterOf yours == theirs = Lose
  | otherwise = Win

defeaterOf :: Move -> Move
defeaterOf = succWrap

succWrap :: (Bounded a, Enum a, Eq a) => a -> a
succWrap move
  | move == maxBound = minBound
  | otherwise = succ move
