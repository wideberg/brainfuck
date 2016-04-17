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
current (Program _ (p:ps)) = p

next :: Program -> Program
next (Program beginning (x:ending)) = Program (x:beginning) ending

prev :: Program -> Program
prev (Program (x:beginning) ending) = Program beginning (x:ending)
prev (Program [] ps) = error $ "prev Unexpected: " ++ ps

matchingRightBracket :: Program -> Program
matchingRightBracket prog = b' 0 (next prog) where
  b' :: Integer -> Program -> Program
  b' 0 p  | (current p) == ']' = p
  b' d p  | (current p) == ']' = b' (d-1) (next p)
          | (current p) == '[' = b' (d+1) (next p)
          | otherwise = b' d (next p)
  b' _ p = error $ "foo: " ++ (show p)

--matchingLeftBracket :: Program -> Program
--matchingLeftBracket p = b' 0 p where
--  b' :: Integer -> Program -> Program
--  b' 0
matchingLeftBracket :: Program -> Program
matchingLeftBracket program = b' 0 (prev program) where
  b' :: Int -> Program -> Program
  b' 0 p  | (current p) == '[' = p
  b' d p  | (current p) == '[' = b' (d-1) (prev p)
          | (current p) == ']' = b' (d+1) (prev p)
          | otherwise = b' d (prev p)
