module Main where

import Interpreter

main :: IO ()
main = do
  run "+++[>,+.<-]"
