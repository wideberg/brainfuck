module Foo where

import Char

data Memory = BrainMemory [Char] [Char]

instance Show Memory where
  show (BrainMemory first last) = show last

inc :: Memory -> Memory
inc (BrainMemory first (x:xs)) = BrainMemory first ((incChar x):xs)

incChar :: Char -> Char
incChar x = chr (ord x + 1)

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
