module Interpreter (Status(..), run) where

import IO

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

runState :: State -> IO ()
runState (State mem pc st) = do
  case st of
    Running -> do
      continueRun mem
    Exit -> do
      return ()
    Input -> do
      c <- getChar
      let mem' = set c mem
      continueRun mem'
    Output c -> do
      putChar c
      hFlush stdout
      continueRun mem

  where
    continueRun :: Memory -> IO ()
    continueRun m = do
      let newState = eval (State m pc st)
      runState newState

run :: [Char] -> IO ()
run program = do
  runState (State empty (new program) Running)
