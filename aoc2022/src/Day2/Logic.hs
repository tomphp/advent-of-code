module Day2.Logic (Result (..), calculateScore) where

import Day2.Game (Game, Score)
import qualified Day2.Game as Game

newtype Result = Result {score :: Score}

calculateScore :: [Game] -> Result
calculateScore games = Result $ sum $ Game.scoreGame <$> games
