module Foo where

import Char

data Memory = Tape [Char] [Char]

instance Show Memory where
  show (Tape first last) = show last

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

empty :: Memory
empty = Tape [] [chr 0]

foo = Tape ['1', '3', '5'] ['6', '8', 'A']
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
