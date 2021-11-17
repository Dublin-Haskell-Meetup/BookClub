-- Using the command framework from the earlier section, write a program that prints the first word of each line of its input

import Distribution.Simple.Program.GHC (GhcOptions (ghcOptInputFiles))
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith firstWord
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input, output] -> interactWith function input output
        _ -> putStrLn "error: exactly two arguments needed"
    firstWord = id
