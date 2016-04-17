module Main where

import System.Environment
import IO

import Interpreter

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  [filename] <- getArgs
  program <- readFile filename
  run program
