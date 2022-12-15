module Common.Runner
  ( InputParser,
    Logic,
    Day (..),
    Printer,
    runDay,
    makeParser,
    makeLogic,
    makePrinter,
  )
where

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Error (errorBundlePretty)

newtype InputParser input = InputParser
  { runParser :: forall m. MonadError Text m => Text -> m input
  }

newtype Logic input result = Logic
  { runLogic :: input -> result
  }

newtype Printer result = Printer
  { runPrinter :: result -> IO ()
  }

makeParser :: Parsec Void Text input -> InputParser input
makeParser parser =
  InputParser $
    liftEither
      . first (Text.pack . show . errorBundlePretty)
      . Megaparsec.runParser parser ""

makeLogic :: (input -> result) -> Logic input result
makeLogic = Logic

makePrinter :: (result -> IO ()) -> Printer result
makePrinter = Printer

data Day = forall input result.
  Day
  { parser :: InputParser input,
    logic :: Logic input result,
    printer :: Printer result
  }

runDay :: (MonadError Text m, MonadIO m) => Day -> Text -> m ()
runDay Day {..} content = do
  parsed <- runParser parser content
  let result = runLogic logic parsed
  liftIO $ runPrinter printer result
