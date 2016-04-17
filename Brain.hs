module Main where
import IO
import Interpreter

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  run "+++[>,+.<-]"
