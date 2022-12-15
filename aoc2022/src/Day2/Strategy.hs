module Day2.Strategy (Strategy (..), toGame) where

import Day2.Game
  ( Game (Game, theirs, yours),
    Move,
    Result (Draw, Lose, Win),
    defeaterOf,
    loserTo,
  )

data Strategy = Strategy
  { theirs :: Move,
    requiredResult :: Result
  }
  deriving stock (Eq, Show)

toGame :: Strategy -> Game
toGame Strategy {theirs, requiredResult = Win} = Game {theirs, yours = defeaterOf theirs}
toGame Strategy {theirs, requiredResult = Draw} = Game {theirs, yours = theirs}
toGame Strategy {theirs, requiredResult = Lose} = Game {theirs, yours = loserTo theirs}
