module Interpreter (Status(..), run) where

import Program
import Memory

data Status = Running | Input | Output Char | Exit deriving Show
data State = State {
  memory :: Memory,
  pc :: Program,
  status :: Status
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
--    '-' -> Running
--    _ -> error "Unknown operation (status)"

runState :: State -> IO ()
runState (State mem pc st) = do
  --putStrLn "Running..."
  case st of
    Running -> do
      continueRun mem
    Exit -> do
      putStrLn "received exit"
    Input -> do
      c <- getChar
      let mem' = set c mem
      continueRun mem'
    Output c -> do
      putChar c
      continueRun mem
    _ -> do
      error $ "Unknown eval status" ++ (show st)

  where
    continueRun :: Memory -> IO ()
    continueRun mem = do
      putStrLn $ show (State mem pc st)
      let foo = eval (State mem pc st)
    --  putStrLn "Again..."
      runState foo

run :: [Char] -> IO ()
run program = do
  runState (State empty (new program) Running)
