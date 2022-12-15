module Day1.Parser (input, elf, sack, foodItem) where

import Common.Parser (endOfLineOrFile)
import Data.Text (Text)
import Data.Void (Void)
import Day1.Elf (Calories, Elf (Elf))
import Text.Megaparsec (Parsec, many, some)
import Text.Megaparsec.Char.Lexer

input :: Parsec Void Text [Elf]
input = many elf

elf :: Parsec Void Text Elf
elf = Elf <$> sack

sack :: Parsec Void Text [Calories]
sack = some foodItem <* endOfLineOrFile

foodItem :: Parsec Void Text Calories
foodItem = decimal <* endOfLineOrFile
