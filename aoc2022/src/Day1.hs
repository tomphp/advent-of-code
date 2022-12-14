module Day1 (day) where

import Common.Runner (Day (Day))
import qualified Common.Runner as Runner
import qualified Day1.Logic as Logic
import qualified Day1.Parser as Parser
import qualified Day1.Printer as Printer

day :: Day
day =
  Day
    { parser = Runner.makeParser Parser.input,
      logic = Runner.makeLogic Logic.getStats,
      printer = Runner.makePrinter Printer.printResults
    }
