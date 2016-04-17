module Interpreter (Status(..), run) where

import Program
import Memory

data Status = Running | Input | Output Char | Exit deriving Show
data State = State {
  _memory :: Memory,
  _pc :: Program,
  _status :: Status
} deriving Show

eval :: State -> State
eval (State memory (Program _xs []) _) = State memory (Program _xs []) Exit
eval (State memory p _status) = State memory' ps' status' where

  memory' :: Memory
  memory' = case (current p) of
    '+' -> inc memory
    '-' -> dec memory
    '>' -> forward memory
    '<' -> backward memory
    _ -> memory

  ps' :: Program
  ps' = case (current p) of
    '[' -> case (get memory) of
      '\NUL' -> matchingRightBracket p
      _ -> next p
    ']' -> case (get memory) of
      '\NUL' -> next p
      _ -> matchingLeftBracket p
    _ -> next p

  status' :: Status
  status' = case (current p) of
    '.' -> Output (get memory)
    ',' -> Input
    _ -> Running

run :: (IO Char) -> (Char -> IO ()) -> [Char] -> IO ()
run input output program = do
  runState (State empty (new program) Running) where

    runState :: State -> IO ()
    runState (State mem pc st) = do
      case st of
        Running -> do
          continueRun mem
        Exit -> do
          return ()
        Input -> do
          c <- input
          let mem' = set c mem
          continueRun mem'
        Output c -> do
          output c
          continueRun mem
    
      where
        continueRun :: Memory -> IO ()
        continueRun m = do
          let newState = eval (State m pc st)
          runState newState
