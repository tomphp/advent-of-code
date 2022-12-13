module Main (main) where

import qualified System.Environment as SE

import qualified Day1

main :: IO ()
main = SE.getArgs >>= run

run :: [String] -> IO ()
run []        = putStrLn "number must be provided as argument"
run ("1" : inputFile : []) = Day1.run inputFile
run _         = putStrLn "unknown arguments"