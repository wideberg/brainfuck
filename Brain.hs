module Main where

import Char

data Memory = Tape [Char] [Char]

instance Show Memory where
  show (Tape _beginning ending) = show ending

inc :: Memory -> Memory
--inc (Tape _ []) -> 
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

empty :: Memory
empty = Tape [] [chr 0]

--foo = Tape ['1', '3', '5'] ['6', '8', 'A']

program :: Program
program = "++"

type Program  = [Char]
type Stack = [Program]
data Status = Running | WaitForInput | Output | Exit deriving Show
--data Stack = Stack Program
data State = State {
  memory :: Memory,
  stack :: [Program],
  pc :: Program,
  status :: Status
} deriving Show

eval :: State -> State
--eval (State _ [] (_:_) _ ) = error "Invalid state"
--eval (State _memory _stack [] status) = State _memory _stack [] Exit
eval (State memory stack (p:ps) _status) = State memory' stack' ps' _status where

  memory' :: Memory
  memory' = case p of
    '+' -> inc memory
    '-' -> dec memory
    '>' -> forward memory
    '<' -> backward memory
    _ -> error "Unknown operation"

  stack' :: Stack
  stack' = case p of
    '[' -> (p:ps):(stack)
    _ -> stack

  ps' :: [Char]
  ps' = case p of
    ']' -> head stack
    _ -> ps

run :: State -> IO ()
run (State mem stk pc st) = do
  putStrLn "Running..."
  case st of
    Exit -> do
      putStrLn "received exit"
    _ -> do 
      let foo = eval (State mem stk pc st)
      putStrLn "Again..."
      run foo

main :: IO ()
main = do
  let initState = (State empty [] program Running)
  run initState  
  putStrLn "Exiting..."
--case p of
  --'+' -> inc memory

--data Program = Main [Char] | Sub [Char]
--data Stack = Stack [Program]
-- data Instruction = Inc | Dec | Forward | Backward | Output | Input | Sub [Instruction] deriving Show
--data Program = Program [Instruction]
--instance Num Program where

--progstring :: String
--progstring = "+++-<>.,[+--]"
--
--prog = foldl parser [] progstring
--
--parser :: [Instruction] -> Char -> [Instruction]
--parser is x = (is ++ parse x)
--
--parse :: Char ->
--parsesub (x:xs) = 
--parse :: String -> ([Instruction], String)
--parse [] = ([], [])
--parse (']':xs) = ([], xs)
--parse ('[':xs) = ((Sub sub): parse rest, rest) where
--  (sub, rest) = parse xs
--parse (x:xs) = (parse':(parse xs)) where
--  parse' = case x of
--    '+' -> Inc
--    '-' -> Dec
--    '>' -> Forward
--    '<' -> Backward
--    '.' -> Output
--    ',' -> Input
    --'[' -> 
    --']' -> []

--data Program  [Instruction]
--program :: String
--program = "+++"
--program :: Program
--program = Main "+++"

--run :: (Program, Memory) -> (Program, Memory)
--run :: Memory -> String -> String
--run (program, empty) = 

--eval :: Memory -> [Char] -> Memory
--eval m (c:cs) = case c of
--  '+' -> inc m
--  '-' -> dec m

--main :: IO ()
--main = do
--  putStrLn $ show (inc empty)
