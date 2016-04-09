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

program :: String
program = "+++"

run :: Memory -> String -> String
run mem (p:ps) = ps

eval :: Memory -> [Char] -> Memory
eval m (c:cs) = case c of
  '+' -> inc m
  '-' -> dec m
