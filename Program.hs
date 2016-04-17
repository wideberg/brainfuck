module Program (Program(..), new, current, next, prev, matchingLeftBracket, matchingRightBracket) where
-- Program
data Program = Program {
  beginning:: [Char],
  ending:: [Char]
  } deriving (Show, Eq)

new :: String -> Program
new s = Program [] s

current :: Program -> Char
current (Program _ []) = error "end of program"
current (Program _ (p:_)) = p

next :: Program -> Program
next (Program beg (x:end)) = Program (x:beg) end
next (Program _ []) = error "can't move past end of program"

prev :: Program -> Program
prev (Program (x:beg) end) = Program beg (x:end)
prev (Program [] _) = error $ "can't move past beginning or program"

matchingRightBracket :: Program -> Program
matchingRightBracket prog = b' 0 (next prog) where
  b' :: Integer -> Program -> Program
  b' 0 p  | (current p) == ']' = p
  b' d p  | (current p) == ']' = b' (d-1) (next p)
          | (current p) == '[' = b' (d+1) (next p)
          | otherwise = b' d (next p)

matchingLeftBracket :: Program -> Program
matchingLeftBracket program = b' 0 (prev program) where
  b' :: Int -> Program -> Program
  b' 0 p  | (current p) == '[' = p
  b' d p  | (current p) == '[' = b' (d-1) (prev p)
          | (current p) == ']' = b' (d+1) (prev p)
          | otherwise = b' d (prev p)
