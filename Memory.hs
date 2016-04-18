module Memory (Memory(..), inc, dec, forward, backward, get, set, empty) where

import Data.Char

data Memory = Tape [Char] [Char]

instance Show Memory where
  show (Tape _beginning ending) = show ending

inc :: Memory -> Memory
inc (Tape first (x:xs)) = Tape first ((incChar x):xs)
inc (Tape _ []) = error "can't increase memory after end of tape"

dec :: Memory -> Memory
dec (Tape first (x:xs)) = Tape first ((decChar x):xs)
dec (Tape _ []) = error "can't decrease memory after end of tape"

addToChar :: Int -> Char -> Char
addToChar n c = chr (ord c + n)

incChar :: Char -> Char
incChar = addToChar 1

decChar :: Char -> Char
decChar = addToChar (-1)

forward :: Memory -> Memory
forward (Tape first (x:[])) = Tape (x:first) [chr 0]
forward (Tape first (x:xs)) = Tape (x:first) xs
forward (Tape _ []) = error "can't forward past end of the tape (shouldn't get here)"

backward :: Memory -> Memory
backward (Tape (x:xs) ending) = Tape xs (x:ending)
backward (Tape [] _) = error "can't backward past beginning of tape"

get :: Memory -> Char
get (Tape _ (x:_)) = x
get (Tape _ []) = error "can't get value past end of tape (shouldn't get here)"

set :: Char -> Memory -> Memory
set c (Tape beginning (_:xs)) = Tape beginning (c:xs)
set _ (Tape _ []) = error "can't set value past end of tape (shouldn't get here)"

empty :: Memory
empty = Tape [] [chr 0]

