module Main where

import System.Environment
import IO

import Interpreter

stdoutChar :: Char -> IO ()
stdoutChar s = do
  putChar s
  hFlush stdout

stdinChar :: IO Char
stdinChar = do
  c <- getChar
  return c

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  [filename] <- getArgs
  program <- readFile filename
  run stdinChar stdoutChar program
