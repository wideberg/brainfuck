module Memory (Memory(..), inc, dec, forward, backward, get, set, empty) where

import Char

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

