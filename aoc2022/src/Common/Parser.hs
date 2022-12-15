module Common.Parser (endOfLineOrFile) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof)
import Text.Megaparsec.Char (eol)

endOfLineOrFile :: Parsec Void Text ()
endOfLineOrFile = eof <|> void eol
