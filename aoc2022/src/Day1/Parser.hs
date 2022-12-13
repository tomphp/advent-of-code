module Day1.Parser (parseInput, elf, sack, foodItem) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Void (Void)
import Day1.Elf (Calories, Elf (Elf))
import Text.Megaparsec (ParseErrorBundle, Parsec, eof, many, runParser, some)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer

parseInput :: Text -> Either (ParseErrorBundle Text Void) [Elf]
parseInput = runParser parseElves ""

parseElves :: Parsec Void Text [Elf]
parseElves = many elf

elf :: Parsec Void Text Elf
elf = Elf <$> sack

sack :: Parsec Void Text [Calories]
sack = some foodItem <* endOfLineOrFile

foodItem :: Parsec Void Text Calories
foodItem = decimal <* endOfLineOrFile

endOfLineOrFile :: Parsec Void Text ()
endOfLineOrFile = eof <|> (() <$ eol)
