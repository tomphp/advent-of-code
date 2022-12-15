module Day2.Game
  ( Move (..),
    Result (..),
    Game (..),
    result,
    defeaterOf,
    loserTo,
  )
where

data Move = Rock | Paper | Scissors
  deriving stock (Bounded, Eq, Enum, Show)

data Game = Game {yours :: Move, theirs :: Move}
  deriving stock (Eq, Show)

data Result = Win | Draw | Lose
  deriving stock (Eq, Show)

result :: Game -> Result
result Game {..}
  | yours == theirs = Draw
  | defeaterOf yours == theirs = Lose
  | otherwise = Win

defeaterOf :: Move -> Move
defeaterOf = succWrap

loserTo :: Move -> Move
loserTo = predWrap

succWrap :: (Bounded a, Enum a, Eq a) => a -> a
succWrap move
  | move == maxBound = minBound
  | otherwise = succ move

predWrap :: (Bounded a, Enum a, Eq a) => a -> a
predWrap move
  | move == minBound = maxBound
  | otherwise = pred move
