module Main where

import Char


-- Memory
data Memory = Tape [Char] [Char]

instance Show Memory where
  show (Tape _beginning ending) = show ending

inc :: Memory -> Memory
inc (Tape first (x:xs)) = Tape first ((incChar x):xs)

dec :: Memory -> Memory
dec (Tape first (x:xs)) = Tape first ((decChar x):xs)

addToChar :: Int -> Char -> Char
addToChar n c = chr (ord c + n)

incChar :: Char -> Char
incChar = addToChar 1

decChar :: Char -> Char
decChar = addToChar (-1)

forward :: Memory -> Memory
forward (Tape first (x:[])) = Tape (x:first) [chr 0]
forward (Tape first (x:xs)) = Tape (x:first) xs

backward :: Memory -> Memory
backward (Tape (x:xs) last) = Tape xs (x:last)

get :: Memory -> Char
get (Tape first (x:xs)) = x

set :: Char -> Memory -> Memory
set c (Tape beginning (x:xs)) = Tape beginning (c:xs)

empty :: Memory
empty = Tape [] [chr 0]

-- Program
data Program = Program {
  beginning:: [Char],
  ending:: [Char]
  } deriving Show

current :: Program -> Char
current (Program _ []) = error "end of program"
current (Program _ (p:ps)) = p

next :: Program -> Program
next (Program beginning (x:ending)) = Program (x:beginning) ending
--matchingRightBracket :: Program -> Program
--matchingRightBracket (p:ps) =

-- Interpreter
data Status = Running | Input | Output Char | Exit deriving Show
data State = State {
  memory :: Memory,
  pc :: Program,
  status :: Status
} deriving Show

program :: Program
program = Program [] ",>,>,+<+<+.>.>."

eval :: State -> State
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
    --']' -> head stack
    _ -> next p

  status' :: Status
  status' = case (current p) of
    '.' -> Output (get memory)
    ',' -> Input
    _ -> Running
--    '-' -> Running
--    _ -> error "Unknown operation (status)"

run :: State -> IO ()
run (State mem pc st) = do
  --putStrLn "Running..."
  case st of
    Running -> do
      continueRun mem
    Exit -> do
      putStrLn "received exit"
    Input -> do
--      putStrLn "input: "
      c <- getChar
--      putStrLn $ "got: " ++ [c]
      let mem' = set c mem
      continueRun mem'
    Output c -> do
      putChar c
--      putStrLn $ "output: " ++ [c]
      continueRun mem
    _ -> do
      error $ "Unknown eval status" ++ (show st)

  where
    continueRun :: Memory -> IO ()
    continueRun mem = do
      let foo = eval (State mem pc st)
    --  putStrLn "Again..."
      run foo

main :: IO ()
main = do
  let initState = (State empty program Running)
  run initState  
  --putStrLn "Exiting..."
