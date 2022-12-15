module Day2 (day) where

import Common.Runner (Day (Day))
import qualified Common.Runner as Runner
import qualified Day2.Logic as Logic
import qualified Day2.Parser as Parser
import qualified Day2.Printer as Printer

day :: Day
day =
  Day
    { parser = Runner.makeParser Parser.input,
      logic = Runner.makeLogic Logic.calculateScore,
      printer = Runner.makePrinter Printer.printResults
    }
