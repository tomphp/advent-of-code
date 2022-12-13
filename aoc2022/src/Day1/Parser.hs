module Day1.Parser (parseInput, elf, sack, foodItem) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec (ParseErrorBundle, runParser, many, some, eof)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer

import Day1.Elf(Elf(Elf), Calories)

parseInput :: Text -> Either (ParseErrorBundle Text Void) [Elf]
parseInput = runParser parseElves ""

parseElves :: Parsec Void Text [Elf]
parseElves = many elf

elf ::  Parsec Void Text Elf
elf = Elf <$> sack

sack :: Parsec Void Text [Calories]
sack = some foodItem <* endOfLineOrFile

foodItem :: Parsec Void Text Calories
foodItem = decimal <* endOfLineOrFile

endOfLineOrFile :: Parsec Void Text ()
endOfLineOrFile = eof <|> (() <$ eol)