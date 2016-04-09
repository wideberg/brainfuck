module Foo where

import Char

data Memory = BrainMemory [Char] [Char]

instance Show Memory where
  show (BrainMemory first last) = show last

inc :: Memory -> Memory
inc (BrainMemory first (x:xs)) = BrainMemory first ((incChar x):xs)

dec :: Memory -> Memory
dec (BrainMemory first (x:xs)) = BrainMemory first ((decChar x):xs)

addToChar :: Int -> Char -> Char
addToChar n c = chr (ord c + n)

incChar :: Char -> Char
incChar = addToChar 1

decChar :: Char -> Char
decChar = addToChar (-1)

forward :: Memory -> Memory
forward (BrainMemory first (x:xs)) = BrainMemory (x:first) xs

get :: Memory -> Char
get (BrainMemory first (x:xs)) = x

foo = BrainMemory ['1', '3', '5'] ['6', '8', 'A']
--get foo

-- data Color = Green | Red | Blue | Yellow | White | Poo deriving (Show)

--data Pos = Up | North | East | South | West | Down deriving (Show)

--data Die = Die Color Color Color Color Color Color deriving (Show)

--foo x = x * x

--d = (Die Blue Blue Blue Blue Blue Blue)
--
--bar = show d
--
--getColor :: Die -> Pos -> Color
--getColor (Die up north east south west down) pos = case pos of
--  Up -> up
--  North -> north
--  East -> east
--  South -> south
--  West -> west
--  Down -> down
